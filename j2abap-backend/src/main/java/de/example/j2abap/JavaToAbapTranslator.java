package de.example.j2abap;

import com.github.javaparser.StaticJavaParser;
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

  // -----------------------------
  // Goal:
  // - Produce ABAP that compiles / is executable (REPORT) for snippet mode.
  // - Never "fake" ABAP params (iv_1 etc.). If unsafe -> TODO comment + keep ABAP syntax valid.
  // -----------------------------
  private static final boolean EMIT_TODOS = true;

  // -----------------------------
  // Per-translation state
  // -----------------------------
  private final Map<String, AbapEnum> enumsByJavaName = new HashMap<>();
  private final Map<String, List<String>> enumTypesByConstant = new HashMap<>();

  private final Set<String> knownTypes = new HashSet<>();
  private final Map<String, List<String>> ctorParamsByType = new HashMap<>();
  private final Map<String, Map<String, List<String>>> methodParamsByType = new HashMap<>();

  // Script analysis (snippet/report)
  private final Map<String, ListInfo> inferredLists = new LinkedHashMap<>();
  private final Map<String, MapInfo> inferredMaps = new LinkedHashMap<>();
  private final Set<String> referencedObjectTypes = new LinkedHashSet<>();
  private final Set<String> mainMethodStubs = new LinkedHashSet<>();

  // -----------------------------
  // Public API
  // -----------------------------
  public String translateAuto(String javaCode) {
    String src = javaCode == null ? "" : javaCode.trim();

    // If it looks like a full CompilationUnit AND does not contain top-level statements -> class mode
    if (looksLikeCompilationUnit(src) && !looksLikeMixedScript(src)) {
      return translateClass(src);
    }
    // Otherwise treat as script/snippet (unlimited mixed content)
    return translateSnippet(src);
  }

  public String translateSnippet(String javaCode) {
    return translateScriptExecutable(javaCode);
  }

  public String translateClass(String javaCode) {
    // Keep class mode for your UI as "class skeleton" output
    // (not wrapped as REPORT), but enums and new-translation are improved.
    return translateCompilationUnitToGlobalAbap(javaCode);
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

  // =============================
  // 1) SCRIPT / SNIPPET (EXECUTABLE REPORT)
  // Supports unlimited mixed chunks: enums/classes + statements, any order.
  // =============================
  private String translateScriptExecutable(String src) {
    resetState();

    StringBuilder out = new StringBuilder();

    // 1) Extract all top-level type declarations (enum/class/interface/record) from anywhere
    Extracted ex = extractTopLevelTypeDeclsAnywhere(src == null ? "" : src);

    // 2) Wrap remaining statements into a valid Java CU (dummy class + method)
    String wrapper =
        "class __J2ABAP {\n" +
            ex.extractedTypes +
            "  void __m() {\n" +
            ex.remainingCode + "\n" +
            "  }\n" +
            "}\n";

    CompilationUnit cu;
    BlockStmt scriptBlock;

    try {
      cu = StaticJavaParser.parse(wrapper);
      indexTypesEnumsAndSignatures(cu);

      MethodDeclaration m = cu.findFirst(MethodDeclaration.class, md -> md.getNameAsString().equals("__m")).orElse(null);
      if (m == null || m.getBody().isEmpty()) {
        return reportWithError("Internal parse error: dummy method missing.");
      }
      scriptBlock = m.getBody().get();

      // 3) Analyze script for missing declarations (list/map/object types) + this-calls
      analyzeScript(scriptBlock);

    } catch (Exception e) {
      return reportWithParseError(e);
    }

    // 4) Emit ABAP executable report
    emit(out, 0, "REPORT zjava_to_abap_script.");
    emit(out, 0, "");

    // 4a) Enums -> local interfaces (lif_*)
    emitEnumInterfacesLocal(out);

    // 4b) Emit local classes from provided Java types (best-effort stubs, so NEW compiles)
    emitLocalTypesFromInput(cu, out);

    // 4c) Emit stubs for referenced types that were NOT defined in input
    emitMissingLocalTypeStubs(out);

    // 4d) Main runner class with optional method stubs to allow `this.foo()` compile
    emitMainRunnerSkeleton(out);

    // 4e) START-OF-SELECTION runner
    emit(out, 0, "START-OF-SELECTION.");
    emit(out, 2, "DATA(lo) = NEW lcl_main( ).");
    emit(out, 2, "lo->run( ).");

    // 4f) Now fill run() implementation with declarations + translated statements
    // (We already emitted main class skeleton; we need to append implementations at end.)
    emitMainRunnerImplementation(out, scriptBlock);

    return ensureTrailingNewline(out);
  }

  private String reportWithParseError(Exception e) {
    StringBuilder out = new StringBuilder();
    emit(out, 0, "REPORT zjava_to_abap_script.");
    emit(out, 0, "");
    emit(out, 0, "START-OF-SELECTION.");
    todo(out, 2, "Parse error (Java) in script/mixed input.");
    todo(out, 2, "Hinweis: Falls du Java-Statements + Typen mischst, ist das in Java top-level nicht erlaubt – der Translator wrappiert es intern.");
    todo(out, 2, shortMessage(e));
    return ensureTrailingNewline(out);
  }

  private String reportWithError(String msg) {
    StringBuilder out = new StringBuilder();
    emit(out, 0, "REPORT zjava_to_abap_script.");
    emit(out, 0, "");
    emit(out, 0, "START-OF-SELECTION.");
    todo(out, 2, msg);
    return ensureTrailingNewline(out);
  }

  // =============================
  // 2) CLASS MODE (GLOBAL ABAP SKELETON)
  // Improved enums + new, but not report-wrapped.
  // =============================
  private String translateCompilationUnitToGlobalAbap(String src) {
    resetState();
    StringBuilder out = new StringBuilder();

    try {
      CompilationUnit cu = StaticJavaParser.parse(src == null ? "" : src);
      emitCompilationUnitHeaderComments(cu, out);

      indexTypesEnumsAndSignatures(cu);

      // Enums -> global-ish interfaces zif_*
      emitEnumInterfacesGlobal(out);

      // If only enum file: done
      Optional<ClassOrInterfaceDeclaration> clsOpt = cu.findFirst(ClassOrInterfaceDeclaration.class);
      Optional<RecordDeclaration> recOpt = cu.findFirst(RecordDeclaration.class);

      if (clsOpt.isPresent() && clsOpt.get().isInterface()) {
        out.append(translateInterfaceGlobal(clsOpt.get()));
        return ensureTrailingNewline(out);
      }
      if (recOpt.isPresent()) {
        out.append(translateRecordGlobal(recOpt.get()));
        return ensureTrailingNewline(out);
      }
      if (clsOpt.isPresent()) {
        out.append(translateClassGlobal(clsOpt.get()));
        return ensureTrailingNewline(out);
      }

      return ensureTrailingNewline(out);

    } catch (Exception e) {
      todo(out, 0, "Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/interface/record/enum.");
      todo(out, 0, shortMessage(e));
      return ensureTrailingNewline(out);
    }
  }

  // =========================================================
  // Script analysis: infer list/map decls, referenced object types, this-method stubs
  // =========================================================
  private void analyzeScript(BlockStmt block) {
    // Track declared variables by encountering Java variable declarations
    Set<String> declared = new HashSet<>();

    // 1) Gather declared vars from VariableDeclarationExpr
    block.findAll(VariableDeclarationExpr.class).forEach(vde -> {
      vde.getVariables().forEach(v -> declared.add(v.getNameAsString()));
    });

    // 2) Infer lists/maps and referenced object types and this-method stubs
    for (Statement st : block.getStatements()) {
      scanStatementForInferences(st, declared);
    }
  }

  private void scanStatementForInferences(Statement st, Set<String> declared) {
    if (st == null) return;

    // variable declarations: infer object types from declared type + initializer new
    st.findAll(VariableDeclarationExpr.class).forEach(vde -> {
      for (VariableDeclarator v : vde.getVariables()) {
        String name = v.getNameAsString();
        String declaredType = v.getTypeAsString();
        declared.add(name);

        if ("var".equals(declaredType) && v.getInitializer().isPresent() && v.getInitializer().get().isObjectCreationExpr()) {
          String jt = stripGeneric(v.getInitializer().get().asObjectCreationExpr().getTypeAsString());
          maybeAddReferencedType(jt);
        } else if (looksLikeTypeName(stripGeneric(declaredType)) && !isKnownScalar(stripGeneric(declaredType))) {
          maybeAddReferencedType(stripGeneric(declaredType));
        }

        v.getInitializer().ifPresent(init -> {
          if (init.isObjectCreationExpr()) {
            String jt = stripGeneric(init.asObjectCreationExpr().getTypeAsString());
            maybeAddReferencedType(jt);
          }
        });
      }
    });

    // method calls: list/map usage + this-method stubs
    st.findAll(MethodCallExpr.class).forEach(mc -> {
      String name = mc.getNameAsString();

      // this.foo() or unqualified foo()
      boolean isThisOrUnqualified =
          mc.getScope().isEmpty() || (mc.getScope().isPresent() && mc.getScope().get().isThisExpr());

      if (isThisOrUnqualified) {
        // stub only if argless (so we can compile safely)
        if (mc.getArguments().isEmpty()) {
          mainMethodStubs.add(name);
        } else {
          // if it has args we cannot safely map, but still add stub to avoid "unknown method" if user later wants manual
          mainMethodStubs.add(name);
        }
      }

      // list / map inference only if scope is a variable name
      if (mc.getScope().isPresent() && mc.getScope().get().isNameExpr()) {
        String var = mc.getScope().get().asNameExpr().getNameAsString();

        // list.add(x)
        if (name.equals("add") && mc.getArguments().size() == 1) {
          ListInfo li = inferredLists.computeIfAbsent(var, k -> new ListInfo(var));
          String elem = inferScalarFromExpr(mc.getArgument(0));
          li.observeElement(elem);
        }

        // list.get(i) also marks it as list (element type unknown unless previously seen)
        if (name.equals("get") && mc.getArguments().size() == 1) {
          inferredLists.computeIfAbsent(var, k -> new ListInfo(var));
        }

        // map.put(k,v)
        if (name.equals("put") && mc.getArguments().size() == 2) {
          MapInfo mi = inferredMaps.computeIfAbsent(var, k -> new MapInfo(var));
          mi.observeKey(inferScalarFromExpr(mc.getArgument(0)));
          mi.observeValue(inferScalarFromExpr(mc.getArgument(1)));
        }

        // map.get(k)
        if (name.equals("get") && mc.getArguments().size() == 1) {
          inferredMaps.computeIfAbsent(var, k -> new MapInfo(var));
        }
      }
    });

    // new expressions
    st.findAll(ObjectCreationExpr.class).forEach(oce -> {
      String jt = stripGeneric(oce.getTypeAsString());
      maybeAddReferencedType(jt);
    });
  }

  private void maybeAddReferencedType(String javaType) {
    if (javaType == null) return;
    String t = stripGeneric(javaType).trim();
    if (t.isEmpty()) return;

    // ignore scalars and collection types
    if (isKnownScalar(t)) return;
    if (isListBase(t) || isMapBase(t)) return;

    // enums are handled as interfaces (not classes)
    if (enumsByJavaName.containsKey(t)) return;

    // only if looks like a type name
    if (looksLikeTypeName(t)) referencedObjectTypes.add(t);
  }

  // =========================================================
  // ABAP emission (script/report)
  // =========================================================

  private void emitEnumInterfacesLocal(StringBuilder out) {
    if (enumsByJavaName.isEmpty()) return;

    for (AbapEnum en : enumsByJavaName.values().stream().sorted(Comparator.comparing(a -> a.javaName)).toList()) {
      String lif = "lif_" + toSnakeLower(en.javaName);
      emit(out, 0, "\" ---- enum " + en.javaName + " (Java) -> local ABAP interface constants ----");
      emit(out, 0, "INTERFACE " + lif + ".");
      emit(out, 2, "TYPES ty TYPE string.");
      for (String c : en.constants) {
        emit(out, 2, "CONSTANTS c_" + toSnakeLower(c) + " TYPE ty VALUE '" + c + "'.");
      }
      emit(out, 0, "ENDINTERFACE.");
      emit(out, 0, "");
    }
  }

  private void emitEnumInterfacesGlobal(StringBuilder out) {
    if (enumsByJavaName.isEmpty()) return;

    for (AbapEnum en : enumsByJavaName.values().stream().sorted(Comparator.comparing(a -> a.javaName)).toList()) {
      String zif = "zif_" + toSnakeLower(en.javaName);
      emit(out, 0, "\" ---- enum " + en.javaName + " (Java) -> ABAP interface constants ----");
      emit(out, 0, "INTERFACE " + zif + " PUBLIC.");
      emit(out, 2, "TYPES ty TYPE string.");
      for (String c : en.constants) {
        emit(out, 2, "CONSTANTS c_" + toSnakeLower(c) + " TYPE ty VALUE '" + c + "'.");
      }
      emit(out, 0, "ENDINTERFACE.");
      emit(out, 0, "");
    }
  }

  private void emitLocalTypesFromInput(CompilationUnit cu, StringBuilder out) {
    // Translate any provided top-level class/record/interface (except wrapper __J2ABAP)
    // As minimal local stubs so NEW compiles and calls are possible.
    List<ClassOrInterfaceDeclaration> classes = cu.findAll(ClassOrInterfaceDeclaration.class).stream()
        .filter(c -> !c.getNameAsString().equals("__J2ABAP"))
        .toList();

    List<RecordDeclaration> records = cu.findAll(RecordDeclaration.class).stream()
        .filter(r -> !r.getNameAsString().equals("__J2ABAP"))
        .toList();

    List<ClassOrInterfaceDeclaration> interfaces = classes.stream().filter(ClassOrInterfaceDeclaration::isInterface).toList();
    List<ClassOrInterfaceDeclaration> realClasses = classes.stream().filter(c -> !c.isInterface()).toList();

    // Local interfaces
    for (ClassOrInterfaceDeclaration itf : interfaces) {
      String lif = "lif_" + toSnakeLower(itf.getNameAsString());
      emit(out, 0, "\" ---- interface " + itf.getNameAsString() + " (Java) -> local ABAP interface (stub) ----");
      emit(out, 0, "INTERFACE " + lif + ".");
      for (MethodDeclaration md : itf.getMethods()) {
        emit(out, 2, "METHODS " + md.getNameAsString() + ".");
      }
      emit(out, 0, "ENDINTERFACE.");
      emit(out, 0, "");
    }

    // Local records -> local classes (stub)
    for (RecordDeclaration r : records) {
      String lcl = "lcl_" + toSnakeLower(r.getNameAsString());
      emit(out, 0, "\" ---- record " + r.getNameAsString() + " (Java) -> local ABAP class (stub) ----");
      emit(out, 0, "CLASS " + lcl + " DEFINITION.");
      emit(out, 2, "PUBLIC SECTION.");
      emit(out, 4, "METHODS constructor.");
      emit(out, 0, "ENDCLASS.");
      emit(out, 0, "CLASS " + lcl + " IMPLEMENTATION.");
      emit(out, 2, "METHOD constructor.");
      emit(out, 2, "ENDMETHOD.");
      emit(out, 0, "ENDCLASS.");
      emit(out, 0, "");
    }

    // Local classes (stub with first constructor + method names)
    for (ClassOrInterfaceDeclaration c : realClasses) {
      String lcl = "lcl_" + toSnakeLower(c.getNameAsString());
      emit(out, 0, "\" ---- class " + c.getNameAsString() + " (Java) -> local ABAP class (stub) ----");
      emit(out, 0, "CLASS " + lcl + " DEFINITION.");
      emit(out, 2, "PUBLIC SECTION.");

      // Constructor (always exists in ABAP; keep parameterless stub so NEW compiles)
      emit(out, 4, "METHODS constructor.");

      // Methods (names only, to allow calls compile)
      Set<String> names = c.getMethods().stream().map(MethodDeclaration::getNameAsString).collect(Collectors.toCollection(LinkedHashSet::new));
      for (String mn : names) emit(out, 4, "METHODS " + mn + ".");

      emit(out, 0, "ENDCLASS.");
      emit(out, 0, "CLASS " + lcl + " IMPLEMENTATION.");
      emit(out, 2, "METHOD constructor.");
      emit(out, 2, "ENDMETHOD.");
      for (String mn : names) {
        emit(out, 2, "METHOD " + mn + ".");
        todo(out, 4, "Stub: method body not translated for local type '" + c.getNameAsString() + "'.");
        emit(out, 2, "ENDMETHOD.");
      }
      emit(out, 0, "ENDCLASS.");
      emit(out, 0, "");
    }
  }

  private void emitMissingLocalTypeStubs(StringBuilder out) {
    // Emit stubs for referenced object types not defined in input
    // (so DATA(x)=NEW lcl_type() compiles even if type was only referenced)
    Set<String> defined = new HashSet<>();

    // Types defined in parsed input
    for (String t : knownTypes) defined.add(t);

    for (String jt : referencedObjectTypes) {
      if (defined.contains(jt)) continue; // already emitted as stub from input
      if (jt.equals("__J2ABAP")) continue;

      String lcl = "lcl_" + toSnakeLower(jt);
      emit(out, 0, "\" ---- missing type '" + jt + "' -> generated local ABAP class stub ----");
      emit(out, 0, "CLASS " + lcl + " DEFINITION.");
      emit(out, 2, "PUBLIC SECTION.");
      emit(out, 4, "METHODS constructor.");
      emit(out, 0, "ENDCLASS.");
      emit(out, 0, "CLASS " + lcl + " IMPLEMENTATION.");
      emit(out, 2, "METHOD constructor.");
      emit(out, 2, "ENDMETHOD.");
      emit(out, 0, "ENDCLASS.");
      emit(out, 0, "");
    }
  }

  private void emitMainRunnerSkeleton(StringBuilder out) {
    emit(out, 0, "\" ---- main runner ----");
    emit(out, 0, "CLASS lcl_main DEFINITION.");
    emit(out, 2, "PUBLIC SECTION.");
    emit(out, 4, "METHODS run.");

    // Add stubs so `this.foo()` / `foo()` compiles
    // (We keep them parameterless; if Java had args -> TODO will be emitted at call site.)
    for (String mn : mainMethodStubs) {
      if (mn.equals("__m")) continue;
      if (mn.equals("run")) continue;
      emit(out, 4, "METHODS " + mn + ".");
    }

    emit(out, 0, "ENDCLASS.");
    emit(out, 0, "");
  }

  private void emitMainRunnerImplementation(StringBuilder out, BlockStmt scriptBlock) {
    // Implementation header
    emit(out, 0, "CLASS lcl_main IMPLEMENTATION.");

    // Stub method bodies first (so calls compile)
    for (String mn : mainMethodStubs) {
      if (mn.equals("__m") || mn.equals("run")) continue;
      emit(out, 2, "METHOD " + mn + ".");
      todo(out, 4, "Stub for this/unqualified call. Implement manually if needed.");
      emit(out, 2, "ENDMETHOD.");
      emit(out, 0, "");
    }

    // run() body
    emit(out, 2, "METHOD run.");

    // Auto-declare inferred list/map variables (if user didn't declare them)
    emitInferredCollectionsDecls(out, 4, scriptBlock);

    // Translate statements
    for (Statement st : scriptBlock.getStatements()) {
      translateStatementScript(st, out, 4);
    }

    emit(out, 2, "ENDMETHOD.");
    emit(out, 0, "ENDCLASS.");
    emit(out, 0, "");
  }

  private void emitInferredCollectionsDecls(StringBuilder out, int indent, BlockStmt block) {
    // Find explicitly declared var names
    Set<String> declared = new HashSet<>();
    block.findAll(VariableDeclarationExpr.class).forEach(vde -> vde.getVariables().forEach(v -> declared.add(v.getNameAsString())));

    // Lists
    for (ListInfo li : inferredLists.values()) {
      if (declared.contains(li.var)) continue;

      String elem = li.elementAbapType();
      if ("string".equals(elem)) {
        emit(out, indent, "DATA " + li.var + " TYPE string_table.");
      } else {
        emit(out, indent, "DATA " + li.var + " TYPE STANDARD TABLE OF " + elem + " WITH EMPTY KEY.");
      }
      emit(out, indent, "CLEAR " + li.var + ".");
    }

    // Maps
    for (MapInfo mi : inferredMaps.values()) {
      if (declared.contains(mi.var)) continue;

      String ty = "ty_" + toSnakeLower(mi.var) + "_entry";
      emit(out, indent, "TYPES: BEGIN OF " + ty + ",");
      emit(out, indent, "         key   TYPE " + mi.keyAbapType() + ",");
      emit(out, indent, "         value TYPE " + mi.valueAbapType() + ",");
      emit(out, indent, "       END OF " + ty + ".");
      emit(out, indent, "DATA " + mi.var + " TYPE HASHED TABLE OF " + ty + " WITH UNIQUE KEY key.");
      emit(out, indent, "CLEAR " + mi.var + ".");
    }
  }

  // =========================================================
  // Script statement translation (compile-safe ABAP)
  // =========================================================

  private void translateStatementScript(Statement st, StringBuilder out, int indent) {
    if (st == null) return;
    emitLeadingComments(st, out, indent);

    if (st.isBlockStmt()) {
      for (Statement inner : st.asBlockStmt().getStatements()) translateStatementScript(inner, out, indent);
      return;
    }
    if (st.isEmptyStmt()) return;

    if (st.isExpressionStmt()) {
      translateExpressionStmtScript(st.asExpressionStmt(), out, indent);
      return;
    }

    if (st.isIfStmt()) {
      IfStmt is = st.asIfStmt();
      emit(out, indent, "IF " + exprScript(is.getCondition()) + ".");
      translateStatementScript(is.getThenStmt(), out, indent + 2);
      if (is.getElseStmt().isPresent()) {
        emit(out, indent, "ELSE.");
        translateStatementScript(is.getElseStmt().get(), out, indent + 2);
      }
      emit(out, indent, "ENDIF.");
      return;
    }

    if (st.isWhileStmt()) {
      WhileStmt ws = st.asWhileStmt();
      emit(out, indent, "WHILE " + exprScript(ws.getCondition()) + ".");
      translateStatementScript(ws.getBody(), out, indent + 2);
      emit(out, indent, "ENDWHILE.");
      return;
    }

    if (st.isDoStmt()) {
      DoStmt ds = st.asDoStmt();
      emit(out, indent, "DO.");
      translateStatementScript(ds.getBody(), out, indent + 2);
      emit(out, indent + 2, "IF NOT ( " + exprScript(ds.getCondition()) + " ).");
      emit(out, indent + 4, "EXIT.");
      emit(out, indent + 2, "ENDIF.");
      emit(out, indent, "ENDDO.");
      return;
    }

    if (st.isForEachStmt()) {
      translateForEachStmtScript(st.asForEachStmt(), out, indent);
      return;
    }

    if (st.isForStmt()) {
      translateForStmtScript(st.asForStmt(), out, indent);
      return;
    }

    if (st.isSwitchStmt()) {
      translateSwitchStmtScript(st.asSwitchStmt(), out, indent);
      return;
    }

    if (st.isTryStmt()) {
      translateTryStmtScript(st.asTryStmt(), out, indent);
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

    // Return at top-level run() is illegal -> TODO only
    if (st.isReturnStmt()) {
      todo(out, indent, "return not allowed in ABAP event/method context like this. Skipped.");
      return;
    }

    todo(out, indent, "Unhandled statement: " + st.getClass().getSimpleName());
  }

  private void translateExpressionStmtScript(ExpressionStmt es, StringBuilder out, int indent) {
    Expression e = es.getExpression();
    emitLeadingComments(e, out, indent);

    // variable declarations
    if (e.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = e.asVariableDeclarationExpr();

      for (VariableDeclarator v : vde.getVariables()) {
        String var = v.getNameAsString();
        String declaredType = v.getTypeAsString();
        Optional<Expression> initOpt = v.getInitializer();

        // enum variable type
        String enumType = stripGeneric(declaredType);
        if (enumsByJavaName.containsKey(enumType)) {
          String lif = "lif_" + toSnakeLower(enumType);
          emit(out, indent, "DATA " + var + " TYPE " + lif + "=>ty.");
          initOpt.ifPresent(init -> emit(out, indent, var + " = " + exprScript(init) + "."));
          continue;
        }

        // var + new
        if ("var".equals(declaredType) && initOpt.isPresent() && initOpt.get().isObjectCreationExpr()) {
          ObjectCreationExpr oce = initOpt.get().asObjectCreationExpr();
          emitNewVarDeclScript(var, oce, out, indent);
          continue;
        }

        // new for object types
        if (initOpt.isPresent() && initOpt.get().isObjectCreationExpr()) {
          ObjectCreationExpr oce = initOpt.get().asObjectCreationExpr();
          String jt = stripGeneric(oce.getTypeAsString());

          // list/map "new" -> just clear table, type already based on declared type
          if (isListBase(jt) || isMapBase(jt)) {
            emit(out, indent, emitDataDeclFromJavaTypeScript(var, declaredType) + ".");
            emit(out, indent, "CLEAR " + var + ".");
            continue;
          }

          // object new: prefer DATA(var) = NEW lcl_type( ) if safe
          emitNewVarDeclScript(var, oce, out, indent);
          continue;
        }

        // list/map declared types
        if (looksLikeListType(declaredType)) {
          emit(out, indent, emitDataDeclFromJavaTypeScript(var, declaredType) + ".");
          emit(out, indent, "CLEAR " + var + ".");
          initOpt.ifPresent(init -> todo(out, indent, "Initializer ignored for list; translated as CLEAR."));
          continue;
        }
        if (looksLikeMapType(declaredType)) {
          emit(out, indent, emitDataDeclFromJavaTypeScript(var, declaredType) + ".");
          emit(out, indent, "CLEAR " + var + ".");
          initOpt.ifPresent(init -> todo(out, indent, "Initializer ignored for map; translated as CLEAR."));
          continue;
        }

        // scalar or unknown -> map best-effort
        emit(out, indent, emitDataDeclFromJavaTypeScript(var, declaredType) + ".");
        initOpt.ifPresent(init -> {
          if (init.isMethodCallExpr()) {
            // try safe list.get/map.get patterns
            MethodCallExpr mc = init.asMethodCallExpr();
            if (tryTranslateListGetIntoTargetScript(mc, var, out, indent)) return;
            if (tryTranslateMapGetIntoTargetScript(mc, var, out, indent)) return;
            todo(out, indent, "Initializer method call not translated safely: " + mc);
            emit(out, indent, "CLEAR " + var + ".");
            return;
          }
          emit(out, indent, var + " = " + exprScript(init) + ".");
        });
      }
      return;
    }

    // assignments
    if (e.isAssignExpr()) {
      AssignExpr ae = e.asAssignExpr();

      // Unsafe: chained this.bar.baz etc -> TODO only
      if (ae.getTarget().isFieldAccessExpr() && ae.getTarget().asFieldAccessExpr().getScope().isFieldAccessExpr()) {
        todo(out, indent, "Chained field access not translated safely: " + ae);
        return;
      }

      String target = exprScript(ae.getTarget());

      if (ae.getOperator() == AssignExpr.Operator.ASSIGN) {
        Expression val = ae.getValue();

        if (val.isNullLiteralExpr()) {
          emit(out, indent, "CLEAR " + target + ".");
          return;
        }

        if (val.isObjectCreationExpr()) {
          emitNewAssignScript(target, val.asObjectCreationExpr(), out, indent);
          return;
        }

        if (val.isMethodCallExpr()) {
          MethodCallExpr mc = val.asMethodCallExpr();
          if (tryTranslateListGetIntoTargetScript(mc, target, out, indent)) return;
          if (tryTranslateMapGetIntoTargetScript(mc, target, out, indent)) return;

          todo(out, indent, "Assignment from method call not translated safely: " + mc);
          emit(out, indent, "CLEAR " + target + ".");
          return;
        }

        emit(out, indent, target + " = " + exprScript(val) + ".");
        return;
      }

      // arithmetic compound operators (safe)
      String rhs = exprScript(ae.getValue());
      switch (ae.getOperator()) {
        case PLUS -> emit(out, indent, target + " = " + target + " + " + rhs + ".");
        case MINUS -> emit(out, indent, target + " = " + target + " - " + rhs + ".");
        case MULTIPLY -> emit(out, indent, target + " = " + target + " * " + rhs + ".");
        case DIVIDE -> emit(out, indent, target + " = " + target + " / " + rhs + ".");
        default -> todo(out, indent, "Compound assignment not translated: " + ae.getOperator());
      }
      return;
    }

    // unary ++ / --
    if (e.isUnaryExpr()) {
      UnaryExpr u = e.asUnaryExpr();
      if (u.getOperator() == UnaryExpr.Operator.POSTFIX_INCREMENT || u.getOperator() == UnaryExpr.Operator.PREFIX_INCREMENT) {
        String x = exprScript(u.getExpression());
        emit(out, indent, x + " = " + x + " + 1.");
        return;
      }
      if (u.getOperator() == UnaryExpr.Operator.POSTFIX_DECREMENT || u.getOperator() == UnaryExpr.Operator.PREFIX_DECREMENT) {
        String x = exprScript(u.getExpression());
        emit(out, indent, x + " = " + x + " - 1.");
        return;
      }
    }

    // method call statements
    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();

      if (isSystemOutPrintln(mc)) {
        String arg = !mc.getArguments().isEmpty() ? exprScript(mc.getArgument(0)) : "''";
        emit(out, indent, "WRITE: / " + arg + ".");
        return;
      }
      if (isSystemOutPrint(mc)) {
        String arg = !mc.getArguments().isEmpty() ? exprScript(mc.getArgument(0)) : "''";
        emit(out, indent, "WRITE " + arg + ".");
        return;
      }

      // list.add(x) -> APPEND x TO list.
      if (mc.getScope().isPresent() && mc.getScope().get().isNameExpr() && mc.getNameAsString().equals("add") && mc.getArguments().size() == 1) {
        emit(out, indent, "APPEND " + exprScript(mc.getArgument(0)) + " TO " + mc.getScope().get().asNameExpr().getNameAsString() + ".");
        return;
      }

      // list.clear() -> CLEAR list.
      if (mc.getScope().isPresent() && mc.getScope().get().isNameExpr() && mc.getNameAsString().equals("clear") && mc.getArguments().isEmpty()) {
        emit(out, indent, "CLEAR " + mc.getScope().get().asNameExpr().getNameAsString() + ".");
        return;
      }

      // map.put(k,v) -> INSERT VALUE #( key=... value=... ) INTO TABLE map.
      if (mc.getScope().isPresent() && mc.getScope().get().isNameExpr() && mc.getNameAsString().equals("put") && mc.getArguments().size() == 2) {
        String map = mc.getScope().get().asNameExpr().getNameAsString();
        emit(out, indent, "INSERT VALUE #( key = " + exprScript(mc.getArgument(0)) + " value = " + exprScript(mc.getArgument(1)) + " ) INTO TABLE " + map + ".");
        return;
      }

      // this.foo(); or foo(); (only if argless -> safe)
      if ((mc.getScope().isEmpty() || (mc.getScope().isPresent() && mc.getScope().get().isThisExpr())) && mc.getArguments().isEmpty()) {
        emit(out, indent, "me->" + mc.getNameAsString() + "( ).");
        return;
      }

      // If arguments exist: do not fake ABAP params -> TODO
      todo(out, indent, "Method call not translated (unknown signature / args): " + mc);
      return;
    }

    // standalone new -> no effect in ABAP; skip
    if (e.isObjectCreationExpr()) {
      todo(out, indent, "Standalone new-expression has no direct ABAP statement effect; skipped: " + e);
      return;
    }

    todo(out, indent, "Unhandled expression statement: " + e.getClass().getSimpleName());
  }

  // =========================================================
  // new-translation (SCRIPT): Auto a = new Auto(); -> DATA(a) = NEW lcl_auto( ).
  // =========================================================
  private void emitNewVarDeclScript(String var, ObjectCreationExpr oce, StringBuilder out, int indent) {
    String javaType = stripGeneric(oce.getTypeAsString());
    String abapType = abapLocalClass(javaType);

    // Collection new -> table
    if (isListBase(javaType) || isMapBase(javaType)) {
      emit(out, indent, "DATA " + var + " TYPE string_table.");
      emit(out, indent, "CLEAR " + var + ".");
      todo(out, indent, "Java new for collections modeled as internal table (no object creation).");
      return;
    }

    // Ensure type exists (we emitted stubs for referenced types)
    if (!looksLikeTypeName(javaType)) {
      todo(out, indent, "new on non-type? skipped: " + oce);
      emit(out, indent, "DATA " + var + " TYPE REF TO object.");
      emit(out, indent, "CLEAR " + var + ".");
      return;
    }

    // args safe only if we know constructor signature from parsed input type and argcount matches
    List<String> formals = ctorParamsByType.get(javaType);
    int argc = oce.getArguments().size();

    if (formals != null && formals.size() == argc) {
      // safe NEW with named params
      if (argc == 0) {
        emit(out, indent, "DATA(" + var + ") = NEW " + abapType + "( ).");
      } else {
        emit(out, indent, "DATA(" + var + ") = NEW " + abapType + "(");
        for (int i = 0; i < argc; i++) {
          String line = "  " + formals.get(i) + " = " + exprScript(oce.getArgument(i));
          if (i < argc - 1) line += "";
          emit(out, indent, line);
        }
        emit(out, indent, ").");
      }
      return;
    }

    // If no args: still safe to create (constructor without params exists in our ABAP stubs)
    if (argc == 0) {
      emit(out, indent, "DATA(" + var + ") = NEW " + abapType + "( ).");
      return;
    }

    // args exist but signature unknown -> do NOT fake; keep ABAP valid
    emit(out, indent, "DATA " + var + " TYPE REF TO " + abapType + ".");
    todo(out, indent, "Cannot translate constructor args safely for " + javaType + " (argcount=" + argc + "). Object not created.");
    emit(out, indent, "CLEAR " + var + ".");
  }

  private void emitNewAssignScript(String target, ObjectCreationExpr oce, StringBuilder out, int indent) {
    String javaType = stripGeneric(oce.getTypeAsString());
    String abapType = abapLocalClass(javaType);

    if (isListBase(javaType) || isMapBase(javaType)) {
      emit(out, indent, "CLEAR " + target + ".");
      todo(out, indent, "Assigned new(collection) modeled as CLEAR(table).");
      return;
    }

    int argc = oce.getArguments().size();
    List<String> formals = ctorParamsByType.get(javaType);

    if (argc == 0) {
      emit(out, indent, target + " = NEW " + abapType + "( ).");
      return;
    }

    if (formals != null && formals.size() == argc) {
      emit(out, indent, target + " = NEW " + abapType + "(");
      for (int i = 0; i < argc; i++) {
        emit(out, indent, "  " + formals.get(i) + " = " + exprScript(oce.getArgument(i)));
      }
      emit(out, indent, ").");
      return;
    }

    todo(out, indent, "Cannot translate new with args safely: " + oce);
    emit(out, indent, "CLEAR " + target + ".");
  }

  // =========================================================
  // list.get / map.get => READ TABLE (SCRIPT)
  // =========================================================
  private boolean tryTranslateListGetIntoTargetScript(MethodCallExpr mc, String target, StringBuilder out, int indent) {
    if (mc == null) return false;
    if (!mc.getNameAsString().equals("get")) return false;
    if (mc.getScope().isEmpty() || !mc.getScope().get().isNameExpr()) return false;
    if (mc.getArguments().size() != 1) return false;

    String list = mc.getScope().get().asNameExpr().getNameAsString();
    Expression idxExpr = mc.getArgument(0);

    if (idxExpr.isIntegerLiteralExpr()) {
      int i0 = Integer.parseInt(idxExpr.asIntegerLiteralExpr().getValue());
      emit(out, indent, "READ TABLE " + list + " INDEX " + (i0 + 1) + " INTO " + target + ".");
      emit(out, indent, "IF sy-subrc <> 0. CLEAR " + target + ". ENDIF.");
      return true;
    }

    emit(out, indent, "DATA(lv_idx) = " + exprScript(idxExpr) + " + 1.");
    emit(out, indent, "READ TABLE " + list + " INDEX lv_idx INTO " + target + ".");
    emit(out, indent, "IF sy-subrc <> 0. CLEAR " + target + ". ENDIF.");
    return true;
  }

  private boolean tryTranslateMapGetIntoTargetScript(MethodCallExpr mc, String target, StringBuilder out, int indent) {
    if (mc == null) return false;
    if (!mc.getNameAsString().equals("get")) return false;
    if (mc.getScope().isEmpty() || !mc.getScope().get().isNameExpr()) return false;
    if (mc.getArguments().size() != 1) return false;

    String map = mc.getScope().get().asNameExpr().getNameAsString();
    String key = exprScript(mc.getArgument(0));

    emit(out, indent, "READ TABLE " + map + " WITH TABLE KEY key = " + key + " INTO DATA(ls_entry).");
    emit(out, indent, "IF sy-subrc = 0.");
    emit(out, indent + 2, target + " = ls_entry-value.");
    emit(out, indent, "ELSE.");
    emit(out, indent + 2, "CLEAR " + target + ".");
    emit(out, indent, "ENDIF.");
    return true;
  }

  // =========================================================
  // Script: for/foreach/switch/try
  // =========================================================
  private void translateForEachStmtScript(ForEachStmt fes, StringBuilder out, int indent) {
    String var = fes.getVariable().getVariable(0).getNameAsString();
    String iterable = exprScript(fes.getIterable());
    emit(out, indent, "LOOP AT " + iterable + " INTO DATA(" + var + ").");
    translateStatementScript(fes.getBody(), out, indent + 2);
    emit(out, indent, "ENDLOOP.");
  }

  private void translateForStmtScript(ForStmt fs, StringBuilder out, int indent) {
    Optional<ForLoopPattern> pat = detectSimpleCountingLoop(fs);
    if (pat.isPresent()) {
      ForLoopPattern p = pat.get();
      emit(out, indent, "DO " + p.timesExpr + " TIMES.");
      emit(out, indent + 2, p.varName + " = sy-index - 1.");
      translateStatementScript(fs.getBody(), out, indent + 2);
      emit(out, indent, "ENDDO.");
      return;
    }
    todo(out, indent, "for-loop not translated (non-trivial).");
    translateStatementScript(fs.getBody(), out, indent);
  }

  private void translateSwitchStmtScript(SwitchStmt ss, StringBuilder out, int indent) {
    emit(out, indent, "CASE " + exprScript(ss.getSelector()) + ".");
    for (SwitchEntry se : ss.getEntries()) {
      if (se.getLabels().isEmpty()) {
        emit(out, indent, "  WHEN OTHERS.");
      } else {
        List<String> labels = se.getLabels().stream().map(this::switchLabelScript).toList();
        emit(out, indent, "  WHEN " + String.join(" OR ", labels) + ".");
      }
      for (Statement st : se.getStatements()) {
        if (st.isBreakStmt()) continue;
        translateStatementScript(st, out, indent + 4);
      }
    }
    emit(out, indent, "ENDCASE.");
  }

  private String switchLabelScript(Expression e) {
    if (e == null) return "''";
    // enum constant
    if (e.isFieldAccessExpr()) return exprScript(e);
    if (e.isNameExpr()) return exprScript(e);
    if (e.isStringLiteralExpr()) return "'" + e.asStringLiteralExpr().asString().replace("'", "''") + "'";
    if (e.isIntegerLiteralExpr()) return e.asIntegerLiteralExpr().getValue();
    if (e.isBooleanLiteralExpr()) return e.asBooleanLiteralExpr().getValue() ? "abap_true" : "abap_false";
    return exprScript(e);
  }

  private void translateTryStmtScript(TryStmt ts, StringBuilder out, int indent) {
    emit(out, indent, "TRY.");
    for (Statement st : ts.getTryBlock().getStatements()) translateStatementScript(st, out, indent + 2);

    for (CatchClause cc : ts.getCatchClauses()) {
      String var = cc.getParameter().getNameAsString();
      emit(out, indent, "CATCH cx_root INTO DATA(" + var + ").");
      todo(out, indent + 2, "Java catch(" + cc.getParameter().getTypeAsString() + ") -> ABAP exception mapping unknown.");
      for (Statement st : cc.getBody().getStatements()) translateStatementScript(st, out, indent + 2);
    }

    if (ts.getFinallyBlock().isPresent()) {
      emit(out, indent, "FINALLY.");
      for (Statement st : ts.getFinallyBlock().get().getStatements()) translateStatementScript(st, out, indent + 2);
    }

    emit(out, indent, "ENDTRY.");
  }

  // =========================================================
  // Script expressions (me->, enums, safe)
  // =========================================================
  private String exprScript(Expression e) {
    if (e == null) return "''";

    if (e.isThisExpr()) return "me";
    if (e.isNullLiteralExpr()) return "INITIAL";

    if (e.isNameExpr()) {
      String n = e.asNameExpr().getNameAsString();
      Optional<String> ec = resolveEnumConstantLocalUnqualified(n);
      return ec.orElse(n);
    }

    if (e.isFieldAccessExpr()) {
      return accessScript(e.asFieldAccessExpr());
    }

    if (e.isIntegerLiteralExpr()) return e.asIntegerLiteralExpr().getValue();
    if (e.isLongLiteralExpr()) return e.asLongLiteralExpr().getValue();
    if (e.isDoubleLiteralExpr()) return e.asDoubleLiteralExpr().getValue();
    if (e.isBooleanLiteralExpr()) return e.asBooleanLiteralExpr().getValue() ? "abap_true" : "abap_false";
    if (e.isStringLiteralExpr()) return "'" + e.asStringLiteralExpr().asString().replace("'", "''") + "'";
    if (e.isCharLiteralExpr()) return "'" + e.asCharLiteralExpr().asChar() + "'";
    if (e.isEnclosedExpr()) return "(" + exprScript(e.asEnclosedExpr().getInner()) + ")";

    if (e.isBinaryExpr()) {
      BinaryExpr b = e.asBinaryExpr();

      // null compare -> INITIAL
      if ((b.getOperator() == BinaryExpr.Operator.EQUALS || b.getOperator() == BinaryExpr.Operator.NOT_EQUALS) &&
          (b.getLeft().isNullLiteralExpr() || b.getRight().isNullLiteralExpr())) {
        Expression other = b.getLeft().isNullLiteralExpr() ? b.getRight() : b.getLeft();
        boolean notEq = b.getOperator() == BinaryExpr.Operator.NOT_EQUALS;
        String opnd = exprScript(other);
        return notEq ? (opnd + " IS NOT INITIAL") : (opnd + " IS INITIAL");
      }

      // string concat heuristic
      if (b.getOperator() == BinaryExpr.Operator.PLUS &&
          (b.getLeft().isStringLiteralExpr() || b.getRight().isStringLiteralExpr())) {
        return exprScript(b.getLeft()) + " && " + exprScript(b.getRight());
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
      if (op != null) return exprScript(b.getLeft()) + " " + op + " " + exprScript(b.getRight());
    }

    if (e.isUnaryExpr()) {
      UnaryExpr u = e.asUnaryExpr();
      if (u.getOperator() == UnaryExpr.Operator.LOGICAL_COMPLEMENT) {
        return "NOT ( " + exprScript(u.getExpression()) + " )";
      }
      return exprScript(u.getExpression());
    }

    if (e.isConditionalExpr()) {
      return "COND #( WHEN " + exprScript(e.asConditionalExpr().getCondition()) +
          " THEN " + exprScript(e.asConditionalExpr().getThenExpr()) +
          " ELSE " + exprScript(e.asConditionalExpr().getElseExpr()) + " )";
    }

    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();

      // list.size() -> lines( list )
      if (mc.getScope().isPresent() && mc.getArguments().isEmpty() && mc.getNameAsString().equals("size")) {
        return "lines( " + exprScript(mc.getScope().get()) + " )";
      }
      if (mc.getScope().isPresent() && mc.getArguments().isEmpty() && mc.getNameAsString().equals("isEmpty")) {
        return "lines( " + exprScript(mc.getScope().get()) + " ) = 0";
      }

      if (isSystemOutPrintln(mc) || isSystemOutPrint(mc)) return "''";
      return "(* TODO: method call not safely inlined *)";
    }

    return "(* TODO: expr *)";
  }

  private String accessScript(FieldAccessExpr fa) {
    Expression scope = fa.getScope();
    String name = fa.getNameAsString();

    if (scope.isThisExpr()) return "me->" + name;

    // enum constant Day.FRIDAY -> lif_day=>c_friday
    if (scope.isNameExpr()) {
      String type = scope.asNameExpr().getNameAsString();
      Optional<String> mapped = resolveEnumConstantLocalQualified(type, name);
      if (mapped.isPresent()) return mapped.get();
      return scope.asNameExpr().getNameAsString() + "->" + name;
    }

    // chained field access (unsafe) -> still print something, but warning comes from statement-level checks
    if (scope.isFieldAccessExpr()) {
      return accessScript(scope.asFieldAccessExpr()) + "->" + name;
    }

    return exprScript(scope) + "->" + name;
  }

  private String emitDataDeclFromJavaTypeScript(String var, String javaTypeRaw) {
    String t = stripGeneric(javaTypeRaw);

    if (enumsByJavaName.containsKey(t)) {
      return "DATA " + var + " TYPE lif_" + toSnakeLower(t) + "=>ty";
    }

    // list/map declared types
    if (looksLikeListType(javaTypeRaw)) {
      // try element type from generic
      Optional<Generic> g = parseGeneric(javaTypeRaw);
      if (g.isPresent() && !g.get().args.isEmpty()) {
        String elem = mapScalarType(g.get().args.get(0));
        if ("string".equals(elem)) return "DATA " + var + " TYPE string_table";
        return "DATA " + var + " TYPE STANDARD TABLE OF " + elem + " WITH EMPTY KEY";
      }
      return "DATA " + var + " TYPE string_table";
    }

    if (looksLikeMapType(javaTypeRaw)) {
      // map declared types -> we cannot safely define entry here without a TYPES block.
      // Prefer inferredMap declarations emitted earlier; if user declared it explicitly, treat as TODO.
      return "DATA " + var + " TYPE string";
    }

    // object type
    if (looksLikeTypeName(t) && !isKnownScalar(t)) {
      return "DATA " + var + " TYPE REF TO " + abapLocalClass(t);
    }

    // scalar
    return "DATA " + var + " TYPE " + mapScalarType(t);
  }

  // =========================================================
  // Global class-mode (kept minimal, but improved enums + signatures)
  // =========================================================

  private String translateInterfaceGlobal(ClassOrInterfaceDeclaration itf) {
    StringBuilder out = new StringBuilder();
    String zif = "zif_" + toSnakeLower(itf.getNameAsString());

    emitLeadingComments(itf, out, 0);
    emit(out, 0, "INTERFACE " + zif + " PUBLIC.");

    for (MethodDeclaration md : itf.getMethods()) {
      emit(out, 2, "METHODS " + md.getNameAsString() + ".");
    }

    emit(out, 0, "ENDINTERFACE.");
    emit(out, 0, "");
    return out.toString();
  }

  private String translateRecordGlobal(RecordDeclaration rec) {
    StringBuilder out = new StringBuilder();
    String zcl = "zcl_" + toSnakeLower(rec.getNameAsString());

    emit(out, 0, "CLASS " + zcl + " DEFINITION PUBLIC FINAL CREATE PUBLIC.");
    emit(out, 2, "PUBLIC SECTION.");
    emit(out, 4, "METHODS constructor.");
    emit(out, 0, "ENDCLASS.");
    emit(out, 0, "CLASS " + zcl + " IMPLEMENTATION.");
    emit(out, 2, "METHOD constructor.");
    emit(out, 2, "ENDMETHOD.");
    emit(out, 0, "ENDCLASS.");
    emit(out, 0, "");
    return out.toString();
  }

  private String translateClassGlobal(ClassOrInterfaceDeclaration cls) {
    StringBuilder out = new StringBuilder();
    String zcl = "zcl_" + toSnakeLower(cls.getNameAsString());

    emitLeadingComments(cls, out, 0);

    emit(out, 0, "CLASS " + zcl + " DEFINITION PUBLIC FINAL CREATE PUBLIC.");
    emit(out, 2, "PUBLIC SECTION.");

    // constructor
    emit(out, 4, "METHODS constructor.");

    // fields (best-effort)
    for (FieldDeclaration fd : cls.getFields()) {
      for (VariableDeclarator v : fd.getVariables()) {
        String name = v.getNameAsString();
        String t = stripGeneric(v.getTypeAsString());
        if (looksLikeTypeName(t) && !isKnownScalar(t) && !enumsByJavaName.containsKey(t)) {
          emit(out, 4, "DATA " + name + " TYPE REF TO zcl_" + toSnakeLower(t) + ".");
        } else if (enumsByJavaName.containsKey(t)) {
          emit(out, 4, "DATA " + name + " TYPE zif_" + toSnakeLower(t) + "=>ty.");
        } else {
          emit(out, 4, "DATA " + name + " TYPE " + mapScalarType(t) + ".");
        }
      }
    }

    // methods (names only)
    Set<String> names = cls.getMethods().stream().map(MethodDeclaration::getNameAsString).collect(Collectors.toCollection(LinkedHashSet::new));
    for (String mn : names) emit(out, 4, "METHODS " + mn + ".");

    emit(out, 0, "ENDCLASS.");
    emit(out, 0, "");
    emit(out, 0, "CLASS " + zcl + " IMPLEMENTATION.");
    emit(out, 2, "METHOD constructor.");
    emit(out, 2, "ENDMETHOD.");

    for (String mn : names) {
      emit(out, 2, "METHOD " + mn + ".");
      todo(out, 4, "Method body translation in class-mode is intentionally conservative.");
      emit(out, 2, "ENDMETHOD.");
    }

    emit(out, 0, "ENDCLASS.");
    emit(out, 0, "");
    return out.toString();
  }

  // =========================================================
  // Enum model + resolution
  // =========================================================
  private record AbapEnum(String javaName, List<String> constants) {}

  private Optional<String> resolveEnumConstantLocalQualified(String enumTypeName, String constant) {
    AbapEnum en = enumsByJavaName.get(enumTypeName);
    if (en == null) return Optional.empty();
    if (!en.constants.contains(constant)) return Optional.empty();
    return Optional.of("lif_" + toSnakeLower(enumTypeName) + "=>c_" + toSnakeLower(constant));
  }

  private Optional<String> resolveEnumConstantLocalUnqualified(String constant) {
    List<String> types = enumTypesByConstant.getOrDefault(constant, List.of());
    if (types.size() != 1) return Optional.empty();
    return resolveEnumConstantLocalQualified(types.get(0), constant);
  }

  // =========================================================
  // Indexing: known types, enums, ctor/method signatures
  // =========================================================
  private void indexTypesEnumsAndSignatures(CompilationUnit cu) {
    knownTypes.clear();
    ctorParamsByType.clear();
    methodParamsByType.clear();
    enumsByJavaName.clear();
    enumTypesByConstant.clear();

    // known types
    cu.findAll(ClassOrInterfaceDeclaration.class).forEach(c -> knownTypes.add(c.getNameAsString()));
    cu.findAll(RecordDeclaration.class).forEach(r -> knownTypes.add(r.getNameAsString()));
    cu.findAll(EnumDeclaration.class).forEach(e -> knownTypes.add(e.getNameAsString()));

    // enums
    for (EnumDeclaration ed : cu.findAll(EnumDeclaration.class)) {
      String n = ed.getNameAsString();
      List<String> cs = ed.getEntries().stream().map(EnumConstantDeclaration::getNameAsString).toList();
      enumsByJavaName.put(n, new AbapEnum(n, cs));
      for (String c : cs) enumTypesByConstant.computeIfAbsent(c, k -> new ArrayList<>()).add(n);
    }

    // constructors + methods for classes in this CU
    for (ClassOrInterfaceDeclaration c : cu.findAll(ClassOrInterfaceDeclaration.class)) {
      if (c.isInterface()) continue;
      String type = c.getNameAsString();

      List<ConstructorDeclaration> ctors = c.getConstructors();
      if (!ctors.isEmpty()) {
        ctorParamsByType.put(type, ctors.get(0).getParameters().stream().map(Parameter::getNameAsString).toList());
      } else {
        ctorParamsByType.put(type, List.of());
      }

      // methods (only store if not overloaded; overloads ambiguous for named parameters)
      Map<String, List<List<String>>> tmp = new HashMap<>();
      for (MethodDeclaration md : c.getMethods()) {
        tmp.computeIfAbsent(md.getNameAsString(), k -> new ArrayList<>())
            .add(md.getParameters().stream().map(Parameter::getNameAsString).toList());
      }

      Map<String, List<String>> chosen = new HashMap<>();
      for (Map.Entry<String, List<List<String>>> e : tmp.entrySet()) {
        if (e.getValue().size() == 1) chosen.put(e.getKey(), e.getValue().get(0));
      }
      methodParamsByType.put(type, chosen);
    }

    // records: constructor params = components
    for (RecordDeclaration r : cu.findAll(RecordDeclaration.class)) {
      ctorParamsByType.put(r.getNameAsString(), r.getParameters().stream().map(Parameter::getNameAsString).toList());
    }
  }

  // =========================================================
  // Extraction of top-level type declarations anywhere in script
  // =========================================================
  private record Extracted(String extractedTypes, String remainingCode) {}

  private Extracted extractTopLevelTypeDeclsAnywhere(String src) {
    String s = src == null ? "" : src;
    StringBuilder types = new StringBuilder();
    StringBuilder rest = new StringBuilder();

    int i = 0;
    int depth = 0;
    boolean inStr = false;
    char quote = 0;

    while (i < s.length()) {
      char c = s.charAt(i);

      if (inStr) {
        rest.append(c);
        if (c == '\\') {
          if (i + 1 < s.length()) { rest.append(s.charAt(i + 1)); i += 2; continue; }
        }
        if (c == quote) { inStr = false; quote = 0; }
        i++;
        continue;
      } else if (c == '"' || c == '\'') {
        inStr = true; quote = c;
        rest.append(c);
        i++;
        continue;
      }

      if (c == '{') depth++;
      if (c == '}') depth = Math.max(0, depth - 1);

      if (depth == 0) {
        int hit = hitTopLevelTypeKeyword(s, i);
        if (hit >= 0) {
          int brace = s.indexOf('{', hit);
          if (brace < 0) break;
          int end = findMatchingBrace(s, brace);
          if (end < 0) break;

          String decl = s.substring(hit, end + 1).trim();
          types.append("  ").append(decl).append("\n\n");

          for (int k = hit; k <= end; k++) rest.append(s.charAt(k) == '\n' ? '\n' : ' ');
          i = end + 1;
          continue;
        }
      }

      rest.append(c);
      i++;
    }

    return new Extracted(types.toString(), rest.toString().trim());
  }

  private int hitTopLevelTypeKeyword(String s, int from) {
    int[] hits = new int[]{
        indexOfKeyword(s, "enum", from),
        indexOfKeyword(s, "class", from),
        indexOfKeyword(s, "interface", from),
        indexOfKeyword(s, "record", from)
    };
    int min = -1;
    for (int h : hits) if (h >= 0 && (min < 0 || h < min)) min = h;
    return min;
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
    char quote = 0;

    for (int i = openIdx; i < s.length(); i++) {
      char c = s.charAt(i);

      if (inStr) {
        if (c == '\\') { i++; continue; }
        if (c == quote) { inStr = false; quote = 0; }
        continue;
      } else if (c == '"' || c == '\'') {
        inStr = true; quote = c;
        continue;
      }

      if (c == '{') depth++;
      else if (c == '}') {
        depth--;
        if (depth == 0) return i;
      }
    }
    return -1;
  }

  // =========================================================
  // Helpers: types + inference
  // =========================================================
  private static class ListInfo {
    final String var;
    final Set<String> observed = new LinkedHashSet<>();
    ListInfo(String var) { this.var = var; }
    void observeElement(String abapScalar) { if (abapScalar != null && !abapScalar.isBlank()) observed.add(abapScalar); }
    String elementAbapType() {
      if (observed.isEmpty()) return "string";
      if (observed.size() == 1) return observed.iterator().next();
      return "string";
    }
  }

  private static class MapInfo {
    final String var;
    final Set<String> keys = new LinkedHashSet<>();
    final Set<String> vals = new LinkedHashSet<>();
    MapInfo(String var) { this.var = var; }
    void observeKey(String t) { if (t != null && !t.isBlank()) keys.add(t); }
    void observeValue(String t) { if (t != null && !t.isBlank()) vals.add(t); }
    String keyAbapType() { return keys.size() == 1 ? keys.iterator().next() : "string"; }
    String valueAbapType() { return vals.size() == 1 ? vals.iterator().next() : "string"; }
  }

  private String inferScalarFromExpr(Expression e) {
    if (e == null) return "string";
    if (e.isStringLiteralExpr()) return "string";
    if (e.isIntegerLiteralExpr() || e.isLongLiteralExpr()) return "i";
    if (e.isDoubleLiteralExpr()) return "f";
    if (e.isBooleanLiteralExpr()) return "abap_bool";
    if (e.isCharLiteralExpr()) return "c LENGTH 1";
    return "string";
  }

  private String abapLocalClass(String javaType) {
    return "lcl_" + toSnakeLower(stripGeneric(javaType));
  }

  private String stripGeneric(String t) {
    if (t == null) return "";
    int lt = t.indexOf('<');
    return lt >= 0 ? t.substring(0, lt).trim() : t.trim();
  }

  private boolean looksLikeListType(String javaTypeRaw) {
    Optional<Generic> g = parseGeneric(javaTypeRaw);
    if (g.isPresent()) return isListBase(g.get().base);
    return isListBase(stripGeneric(javaTypeRaw));
  }

  private boolean looksLikeMapType(String javaTypeRaw) {
    Optional<Generic> g = parseGeneric(javaTypeRaw);
    if (g.isPresent()) return isMapBase(g.get().base);
    return isMapBase(stripGeneric(javaTypeRaw));
  }

  private boolean isListBase(String base) {
    String b = stripGeneric(base);
    return b.equals("List") || b.equals("ArrayList") || b.equals("LinkedList") || b.equals("Collection");
  }

  private boolean isMapBase(String base) {
    String b = stripGeneric(base);
    return b.equals("Map") || b.equals("HashMap") || b.equals("LinkedHashMap") || b.equals("TreeMap");
  }

  private boolean isKnownScalar(String t) {
    return switch (t) {
      case "int", "Integer", "long", "Long", "short", "Short", "byte", "Byte",
           "double", "Double", "float", "Float", "boolean", "Boolean", "String" -> true;
      default -> false;
    };
  }

  private boolean looksLikeTypeName(String n) {
    return n != null && !n.isBlank() && Character.isUpperCase(n.charAt(0));
  }

  private String mapScalarType(String javaType) {
    String t = stripGeneric(javaType == null ? "" : javaType.trim());
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

  // =========================================================
  // Generic parsing helper
  // =========================================================
  private record Generic(String base, List<String> args) {}

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

  // =========================================================
  // JavaParser "System.out" detection
  // =========================================================
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

  // =========================================================
  // Comments: Java -> ABAP
  // =========================================================
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

  // =========================================================
  // For loop pattern detection
  // =========================================================
  private record ForLoopPattern(String varName, String timesExpr) {}

  private Optional<ForLoopPattern> detectSimpleCountingLoop(ForStmt fs) {
    if (fs.getInitialization().size() != 1) return Optional.empty();
    if (fs.getCompare().isEmpty()) return Optional.empty();
    if (fs.getUpdate().size() != 1) return Optional.empty();

    String varName;

    Expression init = fs.getInitialization().get(0);
    if (init.isVariableDeclarationExpr()) {
      VariableDeclarator vd = init.asVariableDeclarationExpr().getVariable(0);
      if (vd.getInitializer().isEmpty()) return Optional.empty();
      Expression iv = vd.getInitializer().get();
      if (!iv.isIntegerLiteralExpr() || !"0".equals(iv.asIntegerLiteralExpr().getValue())) return Optional.empty();
      varName = vd.getNameAsString();
    } else if (init.isAssignExpr()) {
      AssignExpr ae = init.asAssignExpr();
      if (!ae.getValue().isIntegerLiteralExpr() || !"0".equals(ae.getValue().asIntegerLiteralExpr().getValue())) return Optional.empty();
      if (!ae.getTarget().isNameExpr()) return Optional.empty();
      varName = ae.getTarget().asNameExpr().getNameAsString();
    } else return Optional.empty();

    Expression cmp = fs.getCompare().get();
    if (!cmp.isBinaryExpr()) return Optional.empty();
    BinaryExpr b = cmp.asBinaryExpr();
    if (b.getOperator() != BinaryExpr.Operator.LESS) return Optional.empty();
    if (!b.getLeft().isNameExpr() || !b.getLeft().asNameExpr().getNameAsString().equals(varName)) return Optional.empty();

    String timesExpr = exprScript(b.getRight());

    Expression upd = fs.getUpdate().get(0);
    boolean ok = false;
    if (upd.isUnaryExpr()) {
      UnaryExpr u = upd.asUnaryExpr();
      ok = (u.getOperator() == UnaryExpr.Operator.POSTFIX_INCREMENT || u.getOperator() == UnaryExpr.Operator.PREFIX_INCREMENT)
          && u.getExpression().isNameExpr()
          && u.getExpression().asNameExpr().getNameAsString().equals(varName);
    } else if (upd.isAssignExpr()) {
      AssignExpr ae = upd.asAssignExpr();
      ok = ae.getOperator() == AssignExpr.Operator.PLUS
          && ae.getTarget().isNameExpr()
          && ae.getTarget().asNameExpr().getNameAsString().equals(varName)
          && ae.getValue().isIntegerLiteralExpr()
          && "1".equals(ae.getValue().asIntegerLiteralExpr().getValue());
    }

    return ok ? Optional.of(new ForLoopPattern(varName, timesExpr)) : Optional.empty();
  }

  // =========================================================
  // Mode detection
  // =========================================================
  private boolean looksLikeCompilationUnit(String src) {
    String s = src == null ? "" : src;
    return s.startsWith("package ")
        || s.startsWith("import ")
        || s.contains("\nimport ")
        || s.contains(" class ")
        || s.contains(" interface ")
        || s.contains(" record ")
        || s.contains(" enum ");
  }

  private boolean looksLikeMixedScript(String src) {
    // Heuristic: contains type decl keyword AND also contains likely statements (semicolon or if/while) outside a class file
    String s = src == null ? "" : src;
    boolean hasType = s.contains("enum ") || s.contains("record ") || s.contains("interface ") || s.contains("class ");
    boolean hasStatements = s.contains(";") || s.contains("if (") || s.contains("while (") || s.contains("for (");
    // top-level enum + statement -> mixed
    return hasType && hasStatements && !s.contains("public class") && !s.contains("package ");
  }

  // =========================================================
  // Reset state
  // =========================================================
  private void resetState() {
    enumsByJavaName.clear();
    enumTypesByConstant.clear();
    knownTypes.clear();
    ctorParamsByType.clear();
    methodParamsByType.clear();
    inferredLists.clear();
    inferredMaps.clear();
    referencedObjectTypes.clear();
    mainMethodStubs.clear();
  }

  // =========================================================
  // Output helpers
  // =========================================================
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

  private String shortMessage(Exception e) {
    String msg = e.getMessage();
    if (msg == null) return e.getClass().getSimpleName();
    String[] lines = msg.split("\n");
    return lines.length > 0 ? lines[0] : msg;
  }
}
