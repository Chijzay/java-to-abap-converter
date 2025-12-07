package de.example.j2abap;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.AccessSpecifier;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.*;
import com.github.javaparser.ast.comments.BlockComment;
import com.github.javaparser.ast.comments.Comment;
import com.github.javaparser.ast.comments.JavadocComment;
import com.github.javaparser.ast.comments.LineComment;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.stmt.*;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

@Service
public class JavaToAbapTranslator {

  /**
   * IMPORTANT DESIGN GOAL:
   * - Generate ABAP that compiles (syntax-correct) as much as possible.
   * - If something cannot be mapped safely (unknown signature, ambiguous construct),
   *   do NOT emit pseudo-ABAP that may break syntax.
   * - Instead: emit a clear ABAP comment line (" TODO: ...") and skip / CLEAR targets.
   */
  private static final boolean EMIT_TODOS = true;

  // ===========================
  // Per-translation state
  // ===========================

  /** Enum name -> enum model */
  private final Map<String, AbapEnum> enumsByJavaName = new HashMap<>();
  /** Enum constant -> [enum types that contain it] */
  private final Map<String, List<String>> enumTypesByConstant = new HashMap<>();

  /** Known types (classes/records/enums) present in the parsed input (when we parse as CU) */
  private final Set<String> knownTypes = new HashSet<>();

  /** Java type -> constructor parameter names (best-effort: first ctor only; ABAP has no overload) */
  private final Map<String, List<String>> ctorParamsByType = new HashMap<>();

  /** Java type -> methodName -> parameter names (best-effort: first method per name; overloads ambiguous) */
  private final Map<String, Map<String, List<String>>> methodParamsByType = new HashMap<>();

  /** Java type -> set of method names that return non-void (for safer expression usage hints) */
  private final Map<String, Set<String>> nonVoidMethodsByType = new HashMap<>();

  // ===========================
  // Public API
  // ===========================

  public String translateAuto(String javaCode) {
    String src = javaCode == null ? "" : javaCode.trim();

    // If it looks like a compilation unit -> class mode
    if (looksLikeCompilationUnit(src)) {
      return translateClass(src);
    }

    // If it begins with enum/class-like declarations but also contains statements afterwards,
    // treat as "mixed snippet" and build a wrapper compilation unit.
    if (looksLikeMixedTopLevelTypes(src)) {
      return translateMixedSnippet(src);
    }

    // Otherwise: plain snippet statements
    return translateSnippet(src);
  }

  public String translateSnippet(String javaCode) {
    return translateSnippetInternal(javaCode);
  }

  public String translateClass(String javaCode) {
    return translateClassInternal(javaCode);
  }

  public String translate(String javaCode, String mode) {
    String src = javaCode == null ? "" : javaCode.trim();
    String m = (mode == null ? "auto" : mode.trim().toLowerCase(Locale.ROOT));
    return switch (m) {
      case "class" -> translateClass(src);
      case "snippet" -> translateSnippet(src);
      default -> translateAuto(src);
    };
  }

  // ===========================
  // SNIPPET (pure statements)
  // ===========================

  private String translateSnippetInternal(String src) {
    StringBuilder abap = new StringBuilder();

    // reset state (no enums/method signatures available in pure snippet mode)
    resetState();

    // Emit executable ABAP report wrapper
    emit(abap, 0, "REPORT zjava_to_abap_snippet.");
    emit(abap, 0, "");
    emit(abap, 0, "START-OF-SELECTION.");

    try {
      BlockStmt block = StaticJavaParser.parseBlock("{\n" + (src == null ? "" : src) + "\n}");
      for (Statement st : block.getStatements()) {
        translateStatement(st, abap, 2);
      }
    } catch (Exception e) {
      todo(abap, 2, "Parse error (Java). Snippet braucht gültige Statements (meist mit ;).");
      todo(abap, 2, shortMessage(e));
    }

    return ensureTrailingNewline(abap);
  }

  // ===========================
  // MIXED SNIPPET (top-level types + statements)
  // Example: "enum Day { ... }\nDay d = Day.FRIDAY; ..."
  // This is NOT valid Java CU. We wrap it into a dummy CU:
  // class __J2ABAP { <enums> void __m(){ <statements> } }
  // ===========================

  private String translateMixedSnippet(String src) {
    StringBuilder abap = new StringBuilder();
    resetState();

    // Extract top-level enum declarations (simple brace matching)
    Extraction ex = extractLeadingTopLevelEnums(src);

    String wrapper =
        "class __J2ABAP {\n" +
            ex.extractedEnums + "\n" +
            "  void __m() {\n" +
            ex.remainingCode + "\n" +
            "  }\n" +
            "}\n";

    try {
      CompilationUnit cu = StaticJavaParser.parse(wrapper);

      // Collect enums & signatures from the wrapper CU
      indexTypesEnumsAndSignatures(cu);

      // Emit executable ABAP report wrapper + enum interface(s)
      emit(abap, 0, "REPORT zjava_to_abap_snippet.");
      emit(abap, 0, "");

      emitEnumInterfaces(abap);

      emit(abap, 0, "START-OF-SELECTION.");

      // Find dummy method statements and translate
      Optional<MethodDeclaration> m = cu.findFirst(MethodDeclaration.class, md -> md.getNameAsString().equals("__m"));
      if (m.isPresent() && m.get().getBody().isPresent()) {
        for (Statement st : m.get().getBody().get().getStatements()) {
          translateStatement(st, abap, 2);
        }
      } else {
        todo(abap, 2, "Internal parse: dummy method missing.");
      }

    } catch (Exception e) {
      emit(abap, 0, "REPORT zjava_to_abap_snippet.");
      emit(abap, 0, "");
      emit(abap, 0, "START-OF-SELECTION.");
      todo(abap, 2, "Parse error (Java). Hinweis: Top-level Statements sind in Java nicht erlaubt.");
      todo(abap, 2, "Tipp: Entweder nur Statements (Snippet) ODER vollständige Java-Datei (class/interface/record/enum).");
      todo(abap, 2, shortMessage(e));
    }

    return ensureTrailingNewline(abap);
  }

  private record Extraction(String extractedEnums, String remainingCode) {}

  private Extraction extractLeadingTopLevelEnums(String src) {
    if (src == null) return new Extraction("", "");
    String s = src;

    StringBuilder enums = new StringBuilder();
    int pos = 0;

    while (true) {
      int i = indexOfKeyword(s, "enum", pos);
      if (i < 0) break;

      // only treat as top-level enum if all preceding chars until i are whitespace/newlines/comments-ish
      // (very simple: if there is any non-whitespace between pos and i, stop)
      if (!s.substring(pos, i).trim().isEmpty()) break;

      int brace = s.indexOf('{', i);
      if (brace < 0) break;

      int end = findMatchingBrace(s, brace);
      if (end < 0) break;

      String enumDecl = s.substring(i, end + 1).trim();
      enums.append("  ").append(enumDecl).append("\n\n");

      pos = end + 1;
    }

    String remaining = s.substring(pos).trim();
    return new Extraction(enums.toString(), remaining);
  }

  private int indexOfKeyword(String s, String kw, int from) {
    if (s == null) return -1;
    int i = s.indexOf(kw, from);
    while (i >= 0) {
      boolean leftOk = (i == 0) || !Character.isJavaIdentifierPart(s.charAt(i - 1));
      int r = i + kw.length();
      boolean rightOk = (r >= s.length()) || !Character.isJavaIdentifierPart(s.charAt(r));
      if (leftOk && rightOk) return i;
      i = s.indexOf(kw, i + 1);
    }
    return -1;
  }

  private int findMatchingBrace(String s, int openIdx) {
    int depth = 0;
    boolean inStr = false;
    char strQuote = 0;

    for (int i = openIdx; i < s.length(); i++) {
      char c = s.charAt(i);

      if (inStr) {
        if (c == '\\') { // skip escaped char
          i++;
          continue;
        }
        if (c == strQuote) {
          inStr = false;
          strQuote = 0;
        }
        continue;
      } else {
        if (c == '"' || c == '\'') {
          inStr = true;
          strQuote = c;
          continue;
        }
      }

      if (c == '{') depth++;
      else if (c == '}') {
        depth--;
        if (depth == 0) return i;
      }
    }
    return -1;
  }

  // ===========================
  // CLASS / INTERFACE / RECORD / ENUM (CompilationUnit)
  // ===========================

  private String translateClassInternal(String src) {
    StringBuilder abap = new StringBuilder();
    resetState();

    try {
      CompilationUnit cu = StaticJavaParser.parse(src);

      emitCompilationUnitHeaderComments(cu, abap);

      indexTypesEnumsAndSignatures(cu);

      // Emit enum interfaces (ABAP alternative to enums)
      emitEnumInterfaces(abap);

      // handle interface
      Optional<ClassOrInterfaceDeclaration> clsOpt = cu.findFirst(ClassOrInterfaceDeclaration.class);
      Optional<RecordDeclaration> recOpt = cu.findFirst(RecordDeclaration.class);

      if (clsOpt.isPresent() && clsOpt.get().isInterface()) {
        abap.append(translateInterface(clsOpt.get()));
        return ensureTrailingNewline(abap);
      }

      if (recOpt.isPresent()) {
        abap.append(translateRecord(recOpt.get()));
        return ensureTrailingNewline(abap);
      }

      if (clsOpt.isPresent()) {
        abap.append(translateClassDecl(clsOpt.get()));
        return ensureTrailingNewline(abap);
      }

      // enum-only file (no class/record/interface)
      if (!enumsByJavaName.isEmpty()) {
        // We already emitted enum interfaces; that's the translation.
        return ensureTrailingNewline(abap);
      }

      todo(abap, 0, "Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/interface/record/enum.");
      return ensureTrailingNewline(abap);

    } catch (Exception e) {
      todo(abap, 0, "Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/interface/record/enum.");
      todo(abap, 0, shortMessage(e));
      return ensureTrailingNewline(abap);
    }
  }

  private void resetState() {
    enumsByJavaName.clear();
    enumTypesByConstant.clear();
    knownTypes.clear();
    ctorParamsByType.clear();
    methodParamsByType.clear();
    nonVoidMethodsByType.clear();
  }

  private void indexTypesEnumsAndSignatures(CompilationUnit cu) {
    // known types
    for (ClassOrInterfaceDeclaration c : cu.findAll(ClassOrInterfaceDeclaration.class)) {
      knownTypes.add(c.getNameAsString());
    }
    for (RecordDeclaration r : cu.findAll(RecordDeclaration.class)) {
      knownTypes.add(r.getNameAsString());
    }
    for (EnumDeclaration e : cu.findAll(EnumDeclaration.class)) {
      knownTypes.add(e.getNameAsString());
    }

    // enums
    collectEnums(cu);

    // constructors & methods
    indexSignatures(cu);
  }

  // ===========================
  // INTERFACE translation
  // ===========================

  private String translateInterface(ClassOrInterfaceDeclaration itf) {
    StringBuilder out = new StringBuilder();

    String javaName = itf.getNameAsString();
    String abapName = "zif_" + toSnakeLower(javaName);

    emitLeadingComments(itf, out, 0);

    emit(out, 0, "INTERFACE " + abapName + " PUBLIC.");

    // interface fields -> constants (only literal initializers)
    for (FieldDeclaration fd : itf.getFields()) {
      for (VariableDeclarator v : fd.getVariables()) {
        if (v.getInitializer().isPresent() && isSimpleLiteral(v.getInitializer().get())) {
          emit(out, 2, "CONSTANTS " + v.getNameAsString()
              + " TYPE " + mapScalarType(v.getTypeAsString())
              + " VALUE " + expr(v.getInitializer().get()) + ".");
        } else {
          todo(out, 2, "Interface field '" + v.getNameAsString() + "' not translated (non-literal).");
        }
      }
    }

    // methods signatures
    for (MethodDeclaration md : itf.getMethods()) {
      emitLeadingComments(md, out, 2);

      out.append("  METHODS ").append(md.getNameAsString());

      if (!md.getParameters().isEmpty()) {
        out.append(" IMPORTING");
        for (Parameter p : md.getParameters()) {
          AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), true, Optional.empty());
          out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(ti.abapType);
        }
      }

      if (!md.getType().isVoidType()) {
        AbapTypeInfo rti = mapTypeInfo(md.getTypeAsString(), "rv_result", true, Optional.empty());
        out.append(" RETURNING VALUE(rv_result) TYPE ").append(rti.abapType);
      }

      out.append(".\n");
    }

    emit(out, 0, "ENDINTERFACE.");
    emit(out, 0, "");

    return out.toString();
  }

  // ===========================
  // RECORD translation
  // ===========================

  private String translateRecord(RecordDeclaration rec) {
    StringBuilder out = new StringBuilder();

    String javaName = rec.getNameAsString();
    String abapName = "zcl_" + toSnakeLower(javaName);

    emitLeadingComments(rec, out, 0);

    emit(out, 0, "CLASS " + abapName + " DEFINITION PUBLIC FINAL CREATE PUBLIC.");
    emit(out, 2, "PUBLIC SECTION.");

    todo(out, 4, "record -> class best-effort: components -> READ-ONLY attributes");

    for (Parameter p : rec.getParameters()) {
      AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), false, Optional.empty());
      emitTypePreLines(out, 4, ti);
      emit(out, 4, "DATA " + p.getNameAsString() + " TYPE " + ti.abapType + " READ-ONLY.");
    }

    out.append("    METHODS constructor");
    if (!rec.getParameters().isEmpty()) {
      out.append(" IMPORTING");
      for (Parameter p : rec.getParameters()) {
        AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), true, Optional.empty());
        out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(ti.abapType);
      }
    }
    out.append(".\n");

    emit(out, 0, "ENDCLASS.");
    emit(out, 0, "");

    emit(out, 0, "CLASS " + abapName + " IMPLEMENTATION.");
    emit(out, 2, "METHOD constructor.");
    for (Parameter p : rec.getParameters()) {
      emit(out, 4, "me->" + p.getNameAsString() + " = " + p.getNameAsString() + ".");
    }
    emit(out, 2, "ENDMETHOD.");
    emit(out, 0, "ENDCLASS.");
    emit(out, 0, "");

    return out.toString();
  }

  // ===========================
  // CLASS translation
  // ===========================

  private String translateClassDecl(ClassOrInterfaceDeclaration cls) {
    StringBuilder out = new StringBuilder();

    String javaName = cls.getNameAsString();
    String abapName = "zcl_" + toSnakeLower(javaName);

    emitLeadingComments(cls, out, 0);

    emit(out, 0, "CLASS " + abapName + " DEFINITION PUBLIC FINAL CREATE PUBLIC.");

    Map<Section, List<FieldDeclaration>> fieldsBySec = new EnumMap<>(Section.class);
    Map<Section, List<CallableDeclaration<?>>> methodsBySec = new EnumMap<>(Section.class);
    for (Section s : Section.values()) {
      fieldsBySec.put(s, new ArrayList<>());
      methodsBySec.put(s, new ArrayList<>());
    }

    for (FieldDeclaration fd : cls.getFields()) fieldsBySec.get(sectionOf(fd)).add(fd);

    // Constructors (ABAP: only one)
    List<ConstructorDeclaration> ctors = cls.getConstructors();
    if (ctors.size() > 1) {
      todo(out, 0, "Multiple Java constructors detected; ABAP has only one CONSTRUCTOR (no overload). Using the first one.");
    }
    if (!ctors.isEmpty()) methodsBySec.get(sectionOf(ctors.get(0))).add(ctors.get(0));

    for (MethodDeclaration md : cls.getMethods()) methodsBySec.get(sectionOf(md)).add(md);

    emitSection(out, Section.PUBLIC, fieldsBySec, methodsBySec);
    emitSection(out, Section.PROTECTED, fieldsBySec, methodsBySec);
    emitSection(out, Section.PRIVATE, fieldsBySec, methodsBySec);

    emit(out, 0, "ENDCLASS.");
    emit(out, 0, "");

    // IMPLEMENTATION
    emit(out, 0, "CLASS " + abapName + " IMPLEMENTATION.");

    if (!ctors.isEmpty()) {
      emit(out, 2, "METHOD constructor.");
      for (Statement st : ctors.get(0).getBody().getStatements()) translateStatement(st, out, 4);
      emit(out, 2, "ENDMETHOD.");
      emit(out, 0, "");
    }

    for (MethodDeclaration md : cls.getMethods()) {
      emit(out, 2, "METHOD " + md.getNameAsString() + ".");
      if (md.getBody().isPresent()) {
        for (Statement st : md.getBody().get().getStatements()) translateStatement(st, out, 4);
      } else {
        todo(out, 4, "Method body missing.");
      }
      emit(out, 2, "ENDMETHOD.");
      emit(out, 0, "");
    }

    emit(out, 0, "ENDCLASS.");
    emit(out, 0, "");

    return out.toString();
  }

  private enum Section { PUBLIC, PROTECTED, PRIVATE }

  private Section sectionOf(BodyDeclaration<?> d) {
    if (d == null) return Section.PUBLIC;

    AccessSpecifier a;
    if (d instanceof FieldDeclaration fd) a = fd.getAccessSpecifier();
    else if (d instanceof MethodDeclaration md) a = md.getAccessSpecifier();
    else if (d instanceof ConstructorDeclaration cd) a = cd.getAccessSpecifier();
    else if (d instanceof ClassOrInterfaceDeclaration cid) a = cid.getAccessSpecifier();
    else if (d instanceof EnumDeclaration ed) a = ed.getAccessSpecifier();
    else if (d instanceof RecordDeclaration rd) a = rd.getAccessSpecifier();
    else return Section.PROTECTED;

    return switch (a) {
      case PUBLIC -> Section.PUBLIC;
      case PROTECTED -> Section.PROTECTED;
      case PRIVATE -> Section.PRIVATE;
      case NONE -> Section.PROTECTED; // package-private
    };
  }

  private void emitSection(
      StringBuilder out,
      Section sec,
      Map<Section, List<FieldDeclaration>> fieldsBySec,
      Map<Section, List<CallableDeclaration<?>>> methodsBySec
  ) {
    emit(out, 2, switch (sec) {
      case PUBLIC -> "PUBLIC SECTION.";
      case PROTECTED -> "PROTECTED SECTION.";
      case PRIVATE -> "PRIVATE SECTION.";
    });

    boolean any = false;

    // Fields
    for (FieldDeclaration fd : fieldsBySec.getOrDefault(sec, List.of())) {
      any = true;
      emitLeadingComments(fd, out, 4);
      for (VariableDeclarator v : fd.getVariables()) {
        emitFieldDecl(out, 4, v.getNameAsString(), v.getTypeAsString(), v.getInitializer(), fd.isStatic(), fd.isFinal());
      }
    }

    // Methods / constructor signatures
    for (CallableDeclaration<?> cd : methodsBySec.getOrDefault(sec, List.of())) {
      any = true;

      if (cd instanceof ConstructorDeclaration ctor) {
        emitLeadingComments(ctor, out, 4);
        out.append("    METHODS constructor");
        if (!ctor.getParameters().isEmpty()) {
          out.append(" IMPORTING");
          for (Parameter p : ctor.getParameters()) {
            AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), true, Optional.empty());
            out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(ti.abapType);
          }
        }
        out.append(".\n");
        continue;
      }

      if (cd instanceof MethodDeclaration md) {
        emitLeadingComments(md, out, 4);

        String kw = md.isStatic() ? "CLASS-METHODS " : "METHODS ";
        out.append("    ").append(kw).append(md.getNameAsString());

        if (!md.getParameters().isEmpty()) {
          out.append(" IMPORTING");
          for (Parameter p : md.getParameters()) {
            AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), true, Optional.empty());
            out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(ti.abapType);
          }
        }

        if (!md.getType().isVoidType()) {
          AbapTypeInfo rti = mapTypeInfo(md.getTypeAsString(), "rv_result", true, Optional.empty());
          out.append(" RETURNING VALUE(rv_result) TYPE ").append(rti.abapType);
        }

        out.append(".\n");
      }
    }

    if (!any) emit(out, 4, "\" (no members)");
  }

  private void emitFieldDecl(
      StringBuilder out,
      int indent,
      String name,
      String javaType,
      Optional<Expression> initializer,
      boolean isStatic,
      boolean isFinal
  ) {
    AbapTypeInfo ti = mapTypeInfo(javaType, name, false, initializer);

    emitTypePreLines(out, indent, ti);

    // static final literal -> CONSTANTS
    if (isStatic && isFinal && initializer.isPresent() && isSimpleLiteral(initializer.get())) {
      emit(out, indent, "CONSTANTS " + name
          + " TYPE " + mapScalarType(javaType)
          + " VALUE " + expr(initializer.get()) + ".");
      return;
    }

    String kw = isStatic ? "CLASS-DATA " : "DATA ";
    emit(out, indent, kw + name + " TYPE " + ti.abapType + ".");

    // initializer (only safe simple expressions; object creation handled in statement translator)
    if (initializer.isPresent()) {
      Expression init = initializer.get();
      if (isSimpleLiteral(init) || init.isNameExpr() || init.isFieldAccessExpr()) {
        emit(out, indent, name + " = " + expr(init) + ".");
      } else if (!init.isNullLiteralExpr() && EMIT_TODOS) {
        todo(out, indent, "Field initializer not translated (non-trivial): " + init);
      }
    }

    if (isFinal && !isStatic) {
      todo(out, indent, "final instance field: no strict ABAP equivalent (consider READ-ONLY + no setter).");
    }
  }

  // ===========================
  // Enum support (ABAP alternative)
  // ===========================

  private record AbapEnum(String javaName, String abapInterface, List<String> constants) {}

  private void collectEnums(CompilationUnit cu) {
    for (EnumDeclaration ed : cu.findAll(EnumDeclaration.class)) {
      String javaName = ed.getNameAsString();
      String abapItf = "zif_" + toSnakeLower(javaName);
      List<String> constants = ed.getEntries().stream()
          .map(EnumConstantDeclaration::getNameAsString)
          .collect(Collectors.toList());

      enumsByJavaName.put(javaName, new AbapEnum(javaName, abapItf, constants));

      for (String c : constants) {
        enumTypesByConstant.computeIfAbsent(c, k -> new ArrayList<>()).add(javaName);
      }
    }
  }

  private void emitEnumInterfaces(StringBuilder out) {
    if (enumsByJavaName.isEmpty()) return;

    List<AbapEnum> all = enumsByJavaName.values().stream()
        .sorted(Comparator.comparing(a -> a.javaName))
        .collect(Collectors.toList());

    for (AbapEnum en : all) {
      emit(out, 0, "\" ---- enum " + en.javaName + " (Java) -> ABAP interface constants ----");
      emit(out, 0, "INTERFACE " + en.abapInterface + " PUBLIC.");
      emit(out, 2, "TYPES ty TYPE string.");
      for (String c : en.constants) {
        emit(out, 2, "CONSTANTS c_" + toSnakeLower(c) + " TYPE ty VALUE '" + c + "'.");
      }
      emit(out, 0, "ENDINTERFACE.");
      emit(out, 0, "");
    }
  }

  private Optional<String> resolveEnumConstantQualified(String typeName, String constantName) {
    AbapEnum en = enumsByJavaName.get(typeName);
    if (en == null) return Optional.empty();
    if (!en.constants.contains(constantName)) return Optional.empty();
    return Optional.of(en.abapInterface + "=>c_" + toSnakeLower(constantName));
  }

  private Optional<String> resolveEnumConstantUnqualified(String constantName) {
    List<String> types = enumTypesByConstant.getOrDefault(constantName, List.of());
    if (types.size() != 1) return Optional.empty();
    return resolveEnumConstantQualified(types.get(0), constantName);
  }

  // ===========================
  // Signature indexing (for safe "new" and safe method calls)
  // ===========================

  private void indexSignatures(CompilationUnit cu) {
    // classes
    for (ClassOrInterfaceDeclaration c : cu.findAll(ClassOrInterfaceDeclaration.class)) {
      if (c.isInterface()) continue;
      String type = c.getNameAsString();

      // constructors: only first ctor (ABAP no overload)
      List<ConstructorDeclaration> ctors = c.getConstructors();
      if (!ctors.isEmpty()) {
        ctorParamsByType.put(type, ctors.get(0).getParameters().stream().map(Parameter::getNameAsString).collect(Collectors.toList()));
      } else {
        ctorParamsByType.put(type, List.of());
      }

      // methods: if overload exists, we treat it as ambiguous -> do not map calls safely
      Map<String, List<List<String>>> tmp = new HashMap<>();
      Map<String, Set<Boolean>> retNonVoid = new HashMap<>();

      for (MethodDeclaration md : c.getMethods()) {
        tmp.computeIfAbsent(md.getNameAsString(), k -> new ArrayList<>())
            .add(md.getParameters().stream().map(Parameter::getNameAsString).collect(Collectors.toList()));
        retNonVoid.computeIfAbsent(md.getNameAsString(), k -> new HashSet<>())
            .add(!md.getType().isVoidType());
      }

      Map<String, List<String>> chosen = new HashMap<>();
      Set<String> nonVoid = new HashSet<>();

      for (Map.Entry<String, List<List<String>>> e : tmp.entrySet()) {
        String name = e.getKey();
        List<List<String>> variants = e.getValue();
        if (variants.size() == 1) {
          chosen.put(name, variants.get(0));
        } else {
          // overload -> ambiguous -> do not translate argument mapping for this method
        }

        if (retNonVoid.getOrDefault(name, Set.of()).contains(true)) nonVoid.add(name);
      }

      methodParamsByType.put(type, chosen);
      nonVoidMethodsByType.put(type, nonVoid);
    }

    // records
    for (RecordDeclaration r : cu.findAll(RecordDeclaration.class)) {
      String type = r.getNameAsString();
      ctorParamsByType.put(type, r.getParameters().stream().map(Parameter::getNameAsString).collect(Collectors.toList()));
      methodParamsByType.putIfAbsent(type, new HashMap<>());
      nonVoidMethodsByType.putIfAbsent(type, new HashSet<>());
    }
  }

  // ===========================
  // Statements
  // ===========================

  private void translateStatement(Statement st, StringBuilder out, int indent) {
    if (st == null) return;

    emitLeadingComments(st, out, indent);

    if (st.isBlockStmt()) {
      for (Statement inner : st.asBlockStmt().getStatements()) translateStatement(inner, out, indent);
      return;
    }
    if (st.isEmptyStmt()) return;

    if (st.isExpressionStmt()) {
      translateExpressionStmt(st.asExpressionStmt(), out, indent);
      return;
    }

    if (st.isReturnStmt()) {
      // In report snippets, RETURN is legal inside forms/methods, but not at top level of event block.
      // We keep it safe: translate as comment if in snippet mode.
      ReturnStmt rs = st.asReturnStmt();
      if (rs.getExpression().isPresent()) {
        todo(out, indent, "return <expr> not valid in ABAP event block. Expression was: " + expr(rs.getExpression().get()));
      } else {
        todo(out, indent, "return not translated in snippet context.");
      }
      return;
    }

    if (st.isIfStmt()) {
      IfStmt is = st.asIfStmt();
      emit(out, indent, "IF " + expr(is.getCondition()) + ".");
      translateStatement(is.getThenStmt(), out, indent + 2);
      if (is.getElseStmt().isPresent()) {
        emit(out, indent, "ELSE.");
        translateStatement(is.getElseStmt().get(), out, indent + 2);
      }
      emit(out, indent, "ENDIF.");
      return;
    }

    if (st.isWhileStmt()) {
      WhileStmt ws = st.asWhileStmt();
      emit(out, indent, "WHILE " + expr(ws.getCondition()) + ".");
      translateStatement(ws.getBody(), out, indent + 2);
      emit(out, indent, "ENDWHILE.");
      return;
    }

    if (st.isDoStmt()) {
      DoStmt ds = st.asDoStmt();
      emit(out, indent, "DO.");
      translateStatement(ds.getBody(), out, indent + 2);
      emit(out, indent + 2, "IF NOT ( " + expr(ds.getCondition()) + " ).");
      emit(out, indent + 4, "EXIT.");
      emit(out, indent + 2, "ENDIF.");
      emit(out, indent, "ENDDO.");
      return;
    }

    if (st.isForEachStmt()) {
      translateForEachStmt(st.asForEachStmt(), out, indent);
      return;
    }

    if (st.isForStmt()) {
      translateForStmt(st.asForStmt(), out, indent);
      return;
    }

    if (st.isSwitchStmt()) {
      translateSwitchStmt(st.asSwitchStmt(), out, indent);
      return;
    }

    if (st.isTryStmt()) {
      translateTryStmt(st.asTryStmt(), out, indent);
      return;
    }

    if (st.isThrowStmt()) {
      ThrowStmt ts = st.asThrowStmt();
      todo(out, indent, "throw not translated (ABAP exception class unknown). Expr: " + expr(ts.getExpression()));
      return;
    }

    if (st.isBreakStmt()) {
      emit(out, indent, "EXIT.");
      return;
    }

    if (st.isContinueStmt()) {
      emit(out, indent, "CONTINUE.");
      return;
    }

    todo(out, indent, "Unhandled statement: " + st.getClass().getSimpleName());
  }

  // ===========================
  // Expression statements
  // ===========================

  private void translateExpressionStmt(ExpressionStmt es, StringBuilder out, int indent) {
    Expression e = es.getExpression();
    emitLeadingComments(e, out, indent);

    // variable declarations
    if (e.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = e.asVariableDeclarationExpr();

      for (VariableDeclarator v : vde.getVariables()) {
        String name = v.getNameAsString();
        String declaredType = v.getTypeAsString();

        Optional<Expression> initOpt = v.getInitializer();

        // Infer for "var" from initializer type when possible
        String effectiveType = declaredType;
        if ("var".equals(declaredType) && initOpt.isPresent()) {
          Expression init = initOpt.get();
          if (init.isObjectCreationExpr()) effectiveType = init.asObjectCreationExpr().getTypeAsString();
        }

        // If the initializer is "new ..." for a collection, we create ABAP internal table type and CLEAR it.
        AbapTypeInfo ti = mapTypeInfo(effectiveType, name, false, initOpt);
        emitTypePreLines(out, indent, ti);
        emit(out, indent, "DATA " + name + " TYPE " + ti.abapType + ".");

        if (initOpt.isPresent()) {
          Expression init = initOpt.get();

          if (init.isNullLiteralExpr()) {
            emit(out, indent, "CLEAR " + name + ".");
            continue;
          }

          if (init.isObjectCreationExpr()) {
            if (translateNewIntoVar(name, effectiveType, init.asObjectCreationExpr(), out, indent)) {
              continue;
            }
            // not translated -> leave variable initial, but mark
            todo(out, indent, "new " + init.asObjectCreationExpr().getTypeAsString() + "(...) not translated (unknown/unsafe).");
            continue;
          }

          if (init.isMethodCallExpr()) {
            // If we can map list.get/map.get to a safe READ TABLE, do it
            MethodCallExpr mc = init.asMethodCallExpr();
            if (tryTranslateListGetIntoTarget(mc, name, out, indent)) continue;
            if (tryTranslateMapGetIntoTarget(mc, name, out, indent)) continue;

            // unknown method call result -> cannot safely translate as expression
            todo(out, indent, "Initializer method call not translated safely: " + mc);
            emit(out, indent, "CLEAR " + name + ".");
            continue;
          }

          // simple assignment
          emit(out, indent, name + " = " + expr(init) + ".");
        }
      }
      return;
    }

    // assignments
    if (e.isAssignExpr()) {
      AssignExpr ae = e.asAssignExpr();
      String target = expr(ae.getTarget());

      if (ae.getOperator() == AssignExpr.Operator.ASSIGN) {
        Expression val = ae.getValue();

        if (val.isNullLiteralExpr()) {
          emit(out, indent, "CLEAR " + target + ".");
          return;
        }

        if (val.isObjectCreationExpr()) {
          if (translateNewIntoExistingRef(target, val.asObjectCreationExpr(), out, indent)) return;
          todo(out, indent, "new " + val.asObjectCreationExpr().getTypeAsString() + "(...) not translated (unknown/unsafe).");
          return;
        }

        if (val.isMethodCallExpr()) {
          MethodCallExpr mc = val.asMethodCallExpr();
          if (tryTranslateListGetIntoTarget(mc, target, out, indent)) return;
          if (tryTranslateMapGetIntoTarget(mc, target, out, indent)) return;

          todo(out, indent, "Assignment from method call not translated safely: " + mc);
          emit(out, indent, "CLEAR " + target + ".");
          return;
        }

        emit(out, indent, target + " = " + expr(val) + ".");
        return;
      }

      // compound ops (safe arithmetic)
      String rhs = expr(ae.getValue());
      switch (ae.getOperator()) {
        case PLUS -> emit(out, indent, target + " = " + target + " + " + rhs + ".");
        case MINUS -> emit(out, indent, target + " = " + target + " - " + rhs + ".");
        case MULTIPLY -> emit(out, indent, target + " = " + target + " * " + rhs + ".");
        case DIVIDE -> emit(out, indent, target + " = " + target + " / " + rhs + ".");
        default -> {
          todo(out, indent, "Compound assignment not translated: " + ae.getOperator());
        }
      }
      return;
    }

    // unary ++ / --
    if (e.isUnaryExpr()) {
      UnaryExpr u = e.asUnaryExpr();
      if (u.getOperator() == UnaryExpr.Operator.POSTFIX_INCREMENT ||
          u.getOperator() == UnaryExpr.Operator.PREFIX_INCREMENT) {
        String x = expr(u.getExpression());
        emit(out, indent, x + " = " + x + " + 1.");
        return;
      }
      if (u.getOperator() == UnaryExpr.Operator.POSTFIX_DECREMENT ||
          u.getOperator() == UnaryExpr.Operator.PREFIX_DECREMENT) {
        String x = expr(u.getExpression());
        emit(out, indent, x + " = " + x + " - 1.");
        return;
      }
    }

    // method calls as statements
    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();

      if (isSystemOutPrintln(mc)) {
        String arg = !mc.getArguments().isEmpty() ? expr(mc.getArgument(0)) : "''";
        emit(out, indent, "WRITE: / " + arg + ".");
        return;
      }
      if (isSystemOutPrint(mc)) {
        String arg = !mc.getArguments().isEmpty() ? expr(mc.getArgument(0)) : "''";
        emit(out, indent, "WRITE " + arg + ".");
        return;
      }

      // list.add(x) -> APPEND x TO list.
      if (mc.getScope().isPresent() && mc.getNameAsString().equals("add") && mc.getArguments().size() == 1) {
        emit(out, indent, "APPEND " + expr(mc.getArgument(0)) + " TO " + expr(mc.getScope().get()) + ".");
        return;
      }

      // list.clear() -> CLEAR list.
      if (mc.getScope().isPresent() && mc.getNameAsString().equals("clear") && mc.getArguments().isEmpty()) {
        emit(out, indent, "CLEAR " + expr(mc.getScope().get()) + ".");
        return;
      }

      // map.put(k,v) -> INSERT VALUE #( key=... value=... ) INTO TABLE map.
      if (mc.getScope().isPresent() && mc.getNameAsString().equals("put") && mc.getArguments().size() == 2) {
        String map = expr(mc.getScope().get());
        emit(out, indent, "INSERT VALUE #( key = " + expr(mc.getArgument(0)) + " value = " + expr(mc.getArgument(1)) + " ) INTO TABLE " + map + ".");
        return;
      }

      // generic ABAP OO call: ONLY if we can map arguments safely via known signatures
      if (translateMethodCallStatementSafe(mc, out, indent)) return;

      // otherwise: skip
      todo(out, indent, "Method call not translated (unknown signature): " + mc);
      return;
    }

    // object creation as standalone statement: new X(...); -> no effect in ABAP (and unsafe)
    if (e.isObjectCreationExpr()) {
      todo(out, indent, "Standalone 'new' expression not translated (no target variable): " + e);
      return;
    }

    todo(out, indent, "Unhandled expression statement: " + e.getClass().getSimpleName());
  }

  // ===========================
  // Safe method call statement translation
  // - We only generate a call if we know the formal parameter names and arg count matches.
  // - Otherwise: we emit a TODO and do nothing (keeps ABAP syntax valid).
  // ===========================

  private boolean translateMethodCallStatementSafe(MethodCallExpr mc, StringBuilder out, int indent) {
    // Determine owner type if it looks like "Type.method()" (static) OR "var.method()" (unknown)
    // We only safely map in two cases:
    // 1) Unqualified call inside a class method -> me->method(...)  (we don't know current class in snippet, so only in class mode it's relevant)
    // 2) Qualified with scope = NameExpr that is a known type -> static call: zcl_type=>method(...)
    // For instance calls on variables: we don't have type info -> unsafe.

    String method = mc.getNameAsString();
    int argc = mc.getArguments().size();

    // case: static call by type name
    if (mc.getScope().isPresent() && mc.getScope().get().isNameExpr()) {
      String scopeName = mc.getScope().get().asNameExpr().getNameAsString();
      if (looksLikeTypeName(scopeName) && knownTypes.contains(scopeName)) {
        List<String> formals = methodParamsByType.getOrDefault(scopeName, Map.of()).get(method);
        if (formals == null) {
          // overload or unknown
          return false;
        }
        if (formals.size() != argc) {
          todo(out, indent, "Static call arg-count mismatch for " + scopeName + "." + method + ": expected " + formals.size() + ", got " + argc);
          return true; // handled (we emitted todo)
        }
        emit(out, indent, "CALL METHOD zcl_" + toSnakeLower(scopeName) + "=>" + method);
        if (argc > 0) emit(out, indent + 2, "EXPORTING");
        for (int i = 0; i < argc; i++) {
          emit(out, indent + 4, formals.get(i) + " = " + expr(mc.getArgument(i)));
        }
        emit(out, indent, ".");
        return true;
      }
    }

    // case: this.method(...) or unqualified method(...) -> me->method(...)
    // We can only safely map if we have exactly one known top-level class in the CU.
    // (This keeps it deterministic and avoids guessing.)
    String soleClass = getSoleClassTypeIfAny();
    if (soleClass != null) {
      boolean isThisOrUnqualified =
          mc.getScope().isEmpty() ||
              (mc.getScope().isPresent() && mc.getScope().get().isThisExpr());

      if (isThisOrUnqualified) {
        List<String> formals = methodParamsByType.getOrDefault(soleClass, Map.of()).get(method);
        if (formals == null) return false;
        if (formals.size() != argc) {
          todo(out, indent, "Call arg-count mismatch for " + method + ": expected " + formals.size() + ", got " + argc);
          return true;
        }
        emit(out, indent, "CALL METHOD me->" + method);
        if (argc > 0) emit(out, indent + 2, "EXPORTING");
        for (int i = 0; i < argc; i++) {
          emit(out, indent + 4, formals.get(i) + " = " + expr(mc.getArgument(i)));
        }
        emit(out, indent, ".");
        return true;
      }
    }

    return false;
  }

  private String getSoleClassTypeIfAny() {
    // pick only if exactly one non-interface class exists
    List<String> classes = knownTypes.stream()
        .filter(this::looksLikeTypeName)
        .collect(Collectors.toList());
    // This is intentionally conservative; we only use it if it’s unambiguous.
    if (classes.size() == 1) return classes.get(0);
    return null;
  }

  // ===========================
  // new-operator translation (SAFE)
  // Rules:
  // - Collections (List/Map) -> internal table, so "new" becomes CLEAR (or nothing). Never CREATE OBJECT.
  // - Known user-defined type (present in CU) -> CREATE OBJECT with EXPORTING if:
  //    * we know constructor param names AND arg count matches
  //   otherwise: TODO only.
  // - Unknown type -> TODO only.
  // ===========================

  private boolean translateNewIntoVar(String varName, String effectiveJavaType, ObjectCreationExpr oce, StringBuilder out, int indent) {
    String newType = stripGeneric(oce.getTypeAsString());

    // collections as internal tables: "new ArrayList<>()" => CLEAR table
    if (isListBase(newType) || isMapBase(newType)) {
      emit(out, indent, "CLEAR " + varName + ".");
      return true;
    }

    // Known user-defined type -> ABAP class reference
    if (knownTypes.contains(newType)) {
      String abapClass = "zcl_" + toSnakeLower(newType);
      // ensure ref type is plausible: if var was declared as scalar (string/i), skip
      // We can't reliably infer here, so we just do the safe thing: create object only if constructor mapping is safe.
      return translateCreateObject(varName, newType, oce.getArguments(), out, indent, abapClass);
    }

    return false;
  }

  private boolean translateNewIntoExistingRef(String targetVar, ObjectCreationExpr oce, StringBuilder out, int indent) {
    String newType = stripGeneric(oce.getTypeAsString());

    // collections: CLEAR
    if (isListBase(newType) || isMapBase(newType)) {
      emit(out, indent, "CLEAR " + targetVar + ".");
      return true;
    }

    if (knownTypes.contains(newType)) {
      String abapClass = "zcl_" + toSnakeLower(newType);
      return translateCreateObject(targetVar, newType, oce.getArguments(), out, indent, abapClass);
    }

    return false;
  }

  private boolean translateCreateObject(String targetVar, String javaType, List<Expression> actualArgs, StringBuilder out, int indent, String abapClass) {
    List<String> formals = ctorParamsByType.get(javaType);
    if (formals == null) {
      todo(out, indent, "Cannot translate new " + javaType + "(): constructor signature unknown.");
      return true;
    }

    if (formals.size() != actualArgs.size()) {
      todo(out, indent, "Cannot translate new " + javaType + "(): arg-count mismatch. expected " + formals.size() + ", got " + actualArgs.size());
      return true;
    }

    // If args match, emit CREATE OBJECT with EXPORTING.
    if (actualArgs.isEmpty()) {
      emit(out, indent, "CREATE OBJECT " + targetVar + " TYPE " + abapClass + ".");
      return true;
    }

    emit(out, indent, "CREATE OBJECT " + targetVar + " TYPE " + abapClass + " EXPORTING");
    for (int i = 0; i < actualArgs.size(); i++) {
      emit(out, indent + 2, formals.get(i) + " = " + expr(actualArgs.get(i)));
    }
    emit(out, indent, ".");
    return true;
  }

  // ===========================
  // List.get / Map.get => READ TABLE (safe ABAP)
  // ===========================

  private boolean tryTranslateListGetIntoTarget(MethodCallExpr mc, String target, StringBuilder out, int indent) {
    if (mc == null) return false;
    if (!mc.getNameAsString().equals("get")) return false;
    if (mc.getScope().isEmpty()) return false;
    if (mc.getArguments().size() != 1) return false;

    String list = expr(mc.getScope().get());
    Expression idxExpr = mc.getArgument(0);

    // Java 0-based -> ABAP 1-based
    if (idxExpr.isIntegerLiteralExpr()) {
      int i0 = Integer.parseInt(idxExpr.asIntegerLiteralExpr().getValue());
      emit(out, indent, "READ TABLE " + list + " INDEX " + (i0 + 1) + " INTO " + target + ".");
      emit(out, indent, "IF sy-subrc <> 0. CLEAR " + target + ". ENDIF.");
      return true;
    }

    String idx = expr(idxExpr);
    emit(out, indent, "DATA(lv_idx) = " + idx + " + 1.");
    emit(out, indent, "READ TABLE " + list + " INDEX lv_idx INTO " + target + ".");
    emit(out, indent, "IF sy-subrc <> 0. CLEAR " + target + ". ENDIF.");
    return true;
  }

  private boolean tryTranslateMapGetIntoTarget(MethodCallExpr mc, String target, StringBuilder out, int indent) {
    if (mc == null) return false;
    if (!mc.getNameAsString().equals("get")) return false;
    if (mc.getScope().isEmpty()) return false;
    if (mc.getArguments().size() != 1) return false;

    String map = expr(mc.getScope().get());
    String key = expr(mc.getArgument(0));

    emit(out, indent, "READ TABLE " + map + " WITH TABLE KEY key = " + key + " INTO DATA(ls_entry).");
    emit(out, indent, "IF sy-subrc = 0.");
    emit(out, indent + 2, target + " = ls_entry-value.");
    emit(out, indent, "ELSE.");
    emit(out, indent + 2, "CLEAR " + target + ".");
    emit(out, indent, "ENDIF.");
    return true;
  }

  // ===========================
  // TRY/CATCH
  // ===========================

  private void translateTryStmt(TryStmt ts, StringBuilder out, int indent) {
    emit(out, indent, "TRY.");
    for (Statement st : ts.getTryBlock().getStatements()) translateStatement(st, out, indent + 2);

    for (CatchClause cc : ts.getCatchClauses()) {
      String var = cc.getParameter().getNameAsString();
      emit(out, indent, "CATCH cx_root INTO DATA(" + var + ").");
      todo(out, indent + 2, "Java catch(" + cc.getParameter().getTypeAsString() + ") -> ABAP exception mapping unknown.");
      for (Statement st : cc.getBody().getStatements()) translateStatement(st, out, indent + 2);
    }

    if (ts.getFinallyBlock().isPresent()) {
      emit(out, indent, "FINALLY.");
      for (Statement st : ts.getFinallyBlock().get().getStatements()) translateStatement(st, out, indent + 2);
    }

    emit(out, indent, "ENDTRY.");
  }

  // ===========================
  // SWITCH
  // ===========================

  private void translateSwitchStmt(SwitchStmt ss, StringBuilder out, int indent) {
    emit(out, indent, "CASE " + expr(ss.getSelector()) + ".");

    List<SwitchEntry> entries = ss.getEntries();
    int i = 0;
    while (i < entries.size()) {
      SwitchEntry entry = entries.get(i);

      List<String> labels = new ArrayList<>();
      boolean isDefault = entry.getLabels().isEmpty();

      if (!isDefault) labels.addAll(entry.getLabels().stream().map(this::switchLabelExpr).collect(Collectors.toList()));

      List<Statement> stmts = new ArrayList<>(entry.getStatements());
      int j = i;
      while (stmts.isEmpty() && j + 1 < entries.size()) {
        SwitchEntry next = entries.get(j + 1);
        if (!next.getLabels().isEmpty()) labels.addAll(next.getLabels().stream().map(this::switchLabelExpr).collect(Collectors.toList()));
        else isDefault = true;
        stmts = new ArrayList<>(next.getStatements());
        j++;
      }

      if (isDefault || labels.isEmpty()) emit(out, indent, "  WHEN OTHERS.");
      else emit(out, indent, "  WHEN " + String.join(" OR ", labels) + ".");

      for (Statement st : stmts) {
        if (st.isBreakStmt()) continue;
        translateStatement(st, out, indent + 4);
      }

      i = j + 1;
    }

    emit(out, indent, "ENDCASE.");
  }

  private String switchLabelExpr(Expression e) {
    if (e == null) return "''";

    // Day.FRIDAY -> zif_day=>c_friday
    if (e.isFieldAccessExpr()) {
      FieldAccessExpr fa = e.asFieldAccessExpr();
      if (fa.getScope().isNameExpr()) {
        String type = fa.getScope().asNameExpr().getNameAsString();
        String c = fa.getNameAsString();
        Optional<String> mapped = resolveEnumConstantQualified(type, c);
        if (mapped.isPresent()) return mapped.get();
      }
      return expr(e);
    }

    // FRIDAY -> if unique enum constant: zif_day=>c_friday
    if (e.isNameExpr()) {
      Optional<String> mapped = resolveEnumConstantUnqualified(e.asNameExpr().getNameAsString());
      return mapped.orElse("'" + e.asNameExpr().getNameAsString() + "'");
    }

    if (e.isStringLiteralExpr()) return "'" + e.asStringLiteralExpr().asString().replace("'", "''") + "'";
    if (e.isCharLiteralExpr()) return "'" + e.asCharLiteralExpr().asChar() + "'";
    if (e.isIntegerLiteralExpr()) return e.asIntegerLiteralExpr().getValue();
    if (e.isBooleanLiteralExpr()) return e.asBooleanLiteralExpr().getValue() ? "abap_true" : "abap_false";

    return expr(e);
  }

  // ===========================
  // FOR / FOREACH
  // ===========================

  private record ForLoopPattern(String varName, String timesExpr) {}

  private void translateForStmt(ForStmt fs, StringBuilder out, int indent) {
    Optional<ForLoopPattern> pat = detectSimpleCountingLoop(fs);
    if (pat.isPresent()) {
      ForLoopPattern p = pat.get();
      emit(out, indent, "DO " + p.timesExpr + " TIMES.");
      emit(out, indent + 2, p.varName + " = sy-index - 1.");
      translateStatement(fs.getBody(), out, indent + 2);
      emit(out, indent, "ENDDO.");
      return;
    }

    todo(out, indent, "for-loop not translated (non-trivial).");
    translateStatement(fs.getBody(), out, indent);
  }

  private void translateForEachStmt(ForEachStmt fes, StringBuilder out, int indent) {
    String var = fes.getVariable().getVariable(0).getNameAsString();
    String iterable = expr(fes.getIterable());

    String elemType = fes.getVariable().getElementType().asString();
    if ("var".equals(elemType)) {
      emit(out, indent, "LOOP AT " + iterable + " INTO DATA(" + var + ").");
    } else {
      emit(out, indent, "LOOP AT " + iterable + " INTO " + var + ".");
    }

    translateStatement(fes.getBody(), out, indent + 2);
    emit(out, indent, "ENDLOOP.");
  }

  private Optional<ForLoopPattern> detectSimpleCountingLoop(ForStmt fs) {
    if (fs.getInitialization().size() != 1) return Optional.empty();
    if (fs.getCompare().isEmpty()) return Optional.empty();
    if (fs.getUpdate().size() != 1) return Optional.empty();

    String varName;

    Expression init = fs.getInitialization().get(0);
    if (init.isVariableDeclarationExpr()) {
      VariableDeclarator vd = init.asVariableDeclarationExpr().getVariable(0);
      if (vd.getInitializer().isEmpty() || !isZeroLiteral(vd.getInitializer().get())) return Optional.empty();
      varName = vd.getNameAsString();
    } else if (init.isAssignExpr()) {
      AssignExpr ae = init.asAssignExpr();
      if (!isZeroLiteral(ae.getValue())) return Optional.empty();
      varName = simpleName(ae.getTarget()).orElse(null);
      if (varName == null) return Optional.empty();
    } else return Optional.empty();

    Expression cmp = fs.getCompare().get();
    if (!(cmp instanceof BinaryExpr b)) return Optional.empty();
    if (b.getOperator() != BinaryExpr.Operator.LESS) return Optional.empty();
    if (!simpleName(b.getLeft()).orElse("").equals(varName)) return Optional.empty();

    String timesExpr = expr(b.getRight());

    Expression upd = fs.getUpdate().get(0);
    boolean ok = false;

    if (upd.isUnaryExpr()) {
      UnaryExpr u = upd.asUnaryExpr();
      ok = (u.getOperator() == UnaryExpr.Operator.POSTFIX_INCREMENT || u.getOperator() == UnaryExpr.Operator.PREFIX_INCREMENT)
          && simpleName(u.getExpression()).orElse("").equals(varName);
    } else if (upd.isAssignExpr()) {
      AssignExpr ae = upd.asAssignExpr();
      ok = ae.getOperator() == AssignExpr.Operator.PLUS
          && simpleName(ae.getTarget()).orElse("").equals(varName)
          && isOneLiteral(ae.getValue());
    }

    return ok ? Optional.of(new ForLoopPattern(varName, timesExpr)) : Optional.empty();
  }

  private boolean isZeroLiteral(Expression e) {
    return e != null && e.isIntegerLiteralExpr() && "0".equals(e.asIntegerLiteralExpr().getValue());
  }

  private boolean isOneLiteral(Expression e) {
    return e != null && e.isIntegerLiteralExpr() && "1".equals(e.asIntegerLiteralExpr().getValue());
  }

  private Optional<String> simpleName(Expression e) {
    if (e == null) return Optional.empty();
    if (e.isNameExpr()) return Optional.of(e.asNameExpr().getNameAsString());
    return Optional.empty();
  }

  // ===========================
  // Expressions (compile-safe ABAP)
  // ===========================

  private String expr(Expression e) {
    if (e == null) return "''";

    if (e.isThisExpr()) return "me";
    if (e.isSuperExpr()) return "super";
    if (e.isNullLiteralExpr()) return "INITIAL";

    if (e.isNameExpr()) {
      String n = e.asNameExpr().getNameAsString();
      Optional<String> ec = resolveEnumConstantUnqualified(n);
      return ec.orElse(n);
    }

    if (e.isFieldAccessExpr()) {
      return access(e.asFieldAccessExpr());
    }

    if (e.isIntegerLiteralExpr()) return e.asIntegerLiteralExpr().getValue();
    if (e.isLongLiteralExpr()) return e.asLongLiteralExpr().getValue();
    if (e.isDoubleLiteralExpr()) return e.asDoubleLiteralExpr().getValue();
    if (e.isBooleanLiteralExpr()) return e.asBooleanLiteralExpr().getValue() ? "abap_true" : "abap_false";
    if (e.isStringLiteralExpr()) return "'" + e.asStringLiteralExpr().asString().replace("'", "''") + "'";
    if (e.isCharLiteralExpr()) return "'" + e.asCharLiteralExpr().asChar() + "'";
    if (e.isEnclosedExpr()) return "(" + expr(e.asEnclosedExpr().getInner()) + ")";

    if (e.isBinaryExpr()) {
      BinaryExpr b = e.asBinaryExpr();

      // null compare -> INITIAL
      if ((b.getOperator() == BinaryExpr.Operator.EQUALS || b.getOperator() == BinaryExpr.Operator.NOT_EQUALS) &&
          (b.getLeft().isNullLiteralExpr() || b.getRight().isNullLiteralExpr())) {
        Expression other = b.getLeft().isNullLiteralExpr() ? b.getRight() : b.getLeft();
        boolean notEq = b.getOperator() == BinaryExpr.Operator.NOT_EQUALS;
        String opnd = expr(other);
        return notEq ? (opnd + " IS NOT INITIAL") : (opnd + " IS INITIAL");
      }

      // string concatenation heuristic: if either operand is string literal -> use &&
      if (b.getOperator() == BinaryExpr.Operator.PLUS &&
          (b.getLeft().isStringLiteralExpr() || b.getRight().isStringLiteralExpr())) {
        return expr(b.getLeft()) + " && " + expr(b.getRight());
      }

      String op = switch (b.getOperator()) {
        case PLUS -> "+";
        case MINUS -> "-";
        case MULTIPLY -> "*";
        case DIVIDE -> "/";
        case GREATER -> ">";
        case GREATER_EQUALS -> ">=";
        case LESS -> "<";
        case LESS_EQUALS -> "<=";
        case EQUALS -> "=";
        case NOT_EQUALS -> "<>";
        case AND -> "AND";
        case OR -> "OR";
        default -> null;
      };
      if (op != null) return expr(b.getLeft()) + " " + op + " " + expr(b.getRight());
    }

    if (e.isUnaryExpr()) {
      UnaryExpr u = e.asUnaryExpr();
      if (u.getOperator() == UnaryExpr.Operator.LOGICAL_COMPLEMENT) {
        return "NOT ( " + expr(u.getExpression()) + " )";
      }
      return expr(u.getExpression());
    }

    if (e.isConditionalExpr()) {
      return "COND #( WHEN " + expr(e.asConditionalExpr().getCondition()) +
          " THEN " + expr(e.asConditionalExpr().getThenExpr()) +
          " ELSE " + expr(e.asConditionalExpr().getElseExpr()) + " )";
    }

    if (e.isMethodCallExpr()) {
      // Do NOT try to inline method calls into expressions unless we have a proven mapping.
      // Keeping it compile-safe:
      MethodCallExpr mc = e.asMethodCallExpr();

      // list.size() -> lines( list )
      if (mc.getScope().isPresent() && mc.getArguments().isEmpty() && mc.getNameAsString().equals("size")) {
        return "lines( " + expr(mc.getScope().get()) + " )";
      }

      // list.isEmpty() -> lines(...) = 0
      if (mc.getScope().isPresent() && mc.getArguments().isEmpty() && mc.getNameAsString().equals("isEmpty")) {
        return "lines( " + expr(mc.getScope().get()) + " ) = 0";
      }

      // System.out.* in expression -> ''
      if (isSystemOutPrintln(mc) || isSystemOutPrint(mc)) return "''";

      // Everything else: cannot safely inline
      return "(* TODO: method call not inlined safely *)";
    }

    // Unsupported expression kinds:
    return "(* TODO: expr *)";
  }

  /**
   * Field access mapping:
   * - this.x      -> me->x
   * - this.x.y    -> me->x->y
   * - super.x     -> super->x
   * - obj.x       -> obj->x
   * - Day.FRIDAY  -> zif_day=>c_friday (enum)
   * - Class.CONST -> zcl_class=>const (best-effort static)
   */
  private String access(FieldAccessExpr fa) {
    Expression scope = fa.getScope();
    String name = fa.getNameAsString();

    if (scope.isThisExpr()) return "me->" + name;
    if (scope.isSuperExpr()) return "super->" + name;

    // Enum constant Day.FRIDAY
    if (scope.isNameExpr()) {
      String type = scope.asNameExpr().getNameAsString();
      Optional<String> mapped = resolveEnumConstantQualified(type, name);
      if (mapped.isPresent()) return mapped.get();

      // static: Class.CONST
      if (looksLikeTypeName(type)) return "zcl_" + toSnakeLower(type) + "=>" + toSnakeLower(name);

      return type + "->" + name;
    }

    if (scope.isFieldAccessExpr()) {
      String base = access(scope.asFieldAccessExpr());
      if (base.endsWith("=>")) return base + toSnakeLower(name);
      return base + "->" + name;
    }

    return expr(scope) + "->" + name;
  }

  // ===========================
  // Type mapping
  // ===========================

  private record AbapTypeInfo(String abapType, List<String> preLines) {}

  private record Generic(String base, List<String> args) {}

  private AbapTypeInfo mapTypeInfo(String javaTypeRaw, String varName, boolean forSignature, Optional<Expression> initializerOpt) {
    String t = javaTypeRaw == null ? "" : javaTypeRaw.trim();

    // Handle arrays
    if (t.endsWith("[]")) {
      String elem = t.substring(0, t.length() - 2).trim();
      String ab = mapScalarType(elem);
      String tt = forSignature ? ("STANDARD TABLE OF " + ab) : ("STANDARD TABLE OF " + ab + " WITH EMPTY KEY");
      return new AbapTypeInfo(tt, List.of());
    }

    // Handle generics on declared type (e.g. List<String>, Map<String,Integer>)
    Optional<Generic> g = parseGeneric(t);
    if (g.isPresent()) {
      Generic gg = g.get();

      // List<T>
      if (isListBase(gg.base)) {
        String elemJava = gg.args.isEmpty() ? "String" : gg.args.get(0);
        String elemAbap = mapScalarType(elemJava);
        if ("String".equals(elemJava)) return new AbapTypeInfo("string_table", List.of());
        String tt = forSignature ? ("STANDARD TABLE OF " + elemAbap) : ("STANDARD TABLE OF " + elemAbap + " WITH EMPTY KEY");
        return new AbapTypeInfo(tt, List.of());
      }

      // Map<K,V> -> hashed table of entry type
      if (isMapBase(gg.base)) {
        String keyJava = gg.args.size() > 0 ? gg.args.get(0) : "String";
        String valJava = gg.args.size() > 1 ? gg.args.get(1) : "String";
        String keyAbap = mapScalarType(keyJava);
        String valAbap = mapScalarType(valJava);

        String ty = "ty_" + toSnakeLower(varName == null ? "map" : varName) + "_entry";
        List<String> pre = new ArrayList<>();
        pre.add("TYPES: BEGIN OF " + ty + ",");
        pre.add("         key   TYPE " + keyAbap + ",");
        pre.add("         value TYPE " + valAbap + ",");
        pre.add("       END OF " + ty + ".");

        String tt = forSignature ? "any" : ("HASHED TABLE OF " + ty + " WITH UNIQUE KEY key");
        return new AbapTypeInfo(tt, pre);
      }
    }

    // If the declared type is a known user-defined class -> REF TO zcl_<type>
    String base = stripGeneric(t);
    if (knownTypes.contains(base) && looksLikeTypeName(base)) {
      return new AbapTypeInfo("REF TO zcl_" + toSnakeLower(base), List.of());
    }

    // plain scalar
    return new AbapTypeInfo(mapScalarType(t), List.of());
  }

  private Optional<Generic> parseGeneric(String type) {
    if (type == null) return Optional.empty();
    String s = type.trim();
    int lt = s.indexOf('<');
    int gt = s.lastIndexOf('>');
    if (lt < 0 || gt < 0 || gt <= lt) return Optional.empty();
    String base = s.substring(0, lt).trim();
    String inside = s.substring(lt + 1, gt).trim();
    return Optional.of(new Generic(base, splitTopLevelArgs(inside)));
  }

  private List<String> splitTopLevelArgs(String inside) {
    List<String> out = new ArrayList<>();
    if (inside == null || inside.isBlank()) return out;

    int depth = 0;
    StringBuilder cur = new StringBuilder();
    for (int i = 0; i < inside.length(); i++) {
      char c = inside.charAt(i);
      if (c == '<') depth++;
      if (c == '>') depth--;
      if (c == ',' && depth == 0) {
        out.add(cur.toString().trim());
        cur.setLength(0);
      } else {
        cur.append(c);
      }
    }
    if (!cur.toString().isBlank()) out.add(cur.toString().trim());
    return out;
  }

  private boolean isListBase(String base) {
    if (base == null) return false;
    String b = stripGeneric(base).trim();
    return b.equals("List") || b.equals("ArrayList") || b.equals("LinkedList") || b.equals("Collection");
  }

  private boolean isMapBase(String base) {
    if (base == null) return false;
    String b = stripGeneric(base).trim();
    return b.equals("Map") || b.equals("HashMap") || b.equals("LinkedHashMap") || b.equals("TreeMap");
  }

  private String stripGeneric(String t) {
    if (t == null) return "";
    int lt = t.indexOf('<');
    return lt >= 0 ? t.substring(0, lt).trim() : t.trim();
  }

  private void emitTypePreLines(StringBuilder out, int indent, AbapTypeInfo ti) {
    for (String line : ti.preLines) emit(out, indent, line);
  }

  private boolean isSimpleLiteral(Expression e) {
    if (e == null) return false;
    return e.isIntegerLiteralExpr() || e.isBooleanLiteralExpr() || e.isStringLiteralExpr()
        || e.isCharLiteralExpr() || e.isDoubleLiteralExpr() || e.isLongLiteralExpr();
  }

  // ===========================
  // System.out detection
  // ===========================

  private boolean isSystemOutPrintln(MethodCallExpr mc) {
    if (!mc.getNameAsString().equals("println")) return false;
    if (mc.getScope().isEmpty()) return false;

    Expression scope = mc.getScope().get();
    if (scope.isFieldAccessExpr()) {
      FieldAccessExpr fa = scope.asFieldAccessExpr();
      return fa.getNameAsString().equals("out")
          && fa.getScope().isNameExpr()
          && fa.getScope().asNameExpr().getNameAsString().equals("System");
    }
    return false;
  }

  private boolean isSystemOutPrint(MethodCallExpr mc) {
    if (!mc.getNameAsString().equals("print")) return false;
    if (mc.getScope().isEmpty()) return false;

    Expression scope = mc.getScope().get();
    if (scope.isFieldAccessExpr()) {
      FieldAccessExpr fa = scope.asFieldAccessExpr();
      return fa.getNameAsString().equals("out")
          && fa.getScope().isNameExpr()
          && fa.getScope().asNameExpr().getNameAsString().equals("System");
    }
    return false;
  }

  // ===========================
  // Comments (Java -> ABAP)
  // ===========================

  private void emitCompilationUnitHeaderComments(CompilationUnit cu, StringBuilder out) {
    if (cu == null) return;
    cu.getComment().ifPresent(c -> emitComment(c, out, 0));
  }

  private void emitLeadingComments(com.github.javaparser.ast.Node node, StringBuilder out, int indent) {
    if (node == null) return;
    node.getComment().ifPresent(c -> emitComment(c, out, indent));
  }

  private void emitComment(Comment c, StringBuilder out, int indent) {
    if (c == null) return;

    String raw = c.getContent();
    if (raw == null) raw = "";
    raw = raw.replace("\r\n", "\n").replace("\r", "\n").trim();

    if (c instanceof LineComment) {
      emit(out, indent, "\" " + raw.trim());
      return;
    }

    if (c instanceof BlockComment || c instanceof JavadocComment) {
      for (String line : raw.split("\n")) {
        String t = line.strip();
        if (!t.isEmpty()) emit(out, indent, "\" " + t);
      }
    }
  }

  // ===========================
  // Helpers
  // ===========================

  private static void emit(StringBuilder out, int indent, String line) {
    out.append(" ".repeat(Math.max(0, indent))).append(line).append("\n");
  }

  private static void todo(StringBuilder out, int indent, String msg) {
    if (!EMIT_TODOS) return;
    emit(out, indent, "\" TODO: " + msg);
  }

  private String ensureTrailingNewline(StringBuilder out) {
    String s = out.toString().replace("\r\n", "\n").replace("\r", "\n").trim();
    return s.isEmpty() ? "\n" : (s + "\n");
  }

  private boolean looksLikeCompilationUnit(String src) {
    String s = src == null ? "" : src;
    return s.startsWith("package ")
        || s.contains("\nimport ")
        || s.contains(" class ")
        || s.contains(" interface ")
        || s.contains(" record ")
        || s.contains(" enum ")
        || s.startsWith("import ")
        || s.startsWith("public ")
        || s.startsWith("private ")
        || s.startsWith("protected ");
  }

  private boolean looksLikeMixedTopLevelTypes(String src) {
    String s = src == null ? "" : src.stripLeading();
    // Typical: starts with "enum ..." but later contains statements like "Day d = ..."
    return s.startsWith("enum ") || s.startsWith("@") && s.contains("enum ");
  }

  private String mapScalarType(String javaType) {
    String t = javaType == null ? "" : javaType.trim();
    t = stripGeneric(t);

    return switch (t) {
      case "int", "Integer", "long", "Long", "short", "Short", "byte", "Byte" -> "i";
      case "double", "Double", "float", "Float" -> "f";
      case "boolean", "Boolean" -> "abap_bool";
      case "String" -> "string";
      default -> "string";
    };
  }

  private String toSnakeLower(String s) {
    if (s == null || s.isBlank()) return "demo";
    String x = s.replaceAll("([a-z])([A-Z])", "$1_$2");
    return x.toLowerCase(Locale.ROOT);
  }

  private boolean looksLikeTypeName(String n) {
    if (n == null || n.isBlank()) return false;
    return Character.isUpperCase(n.charAt(0));
  }

  private String shortMessage(Exception e) {
    String msg = e.getMessage();
    if (msg == null) return e.getClass().getSimpleName();
    String[] lines = msg.split("\n");
    return lines.length > 0 ? lines[0] : msg;
  }
}
