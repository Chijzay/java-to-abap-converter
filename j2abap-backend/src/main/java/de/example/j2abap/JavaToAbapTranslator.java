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
   * If true: emits helpful ABAP comments for unsupported / ambiguous constructs.
   * If false: tries to keep output clean and ABAP-like.
   */
  private static final boolean EMIT_HINTS = false;

  // Collected enums (per translation run)
  private final Map<String, AbapEnum> enumsByType = new HashMap<>();
  private final Map<String, List<String>> enumTypesByConstant = new HashMap<>();

  // ===========================
  // PUBLIC API
  // ===========================

  public String translateAuto(String javaCode) {
    String src = javaCode == null ? "" : javaCode.trim();
    return looksLikeClass(src) ? translateClass(src) : translateSnippet(src);
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
  // SNIPPET
  // ===========================

  private String translateSnippetInternal(String src) {
    StringBuilder out = new StringBuilder();

    try {
      BlockStmt block = StaticJavaParser.parseBlock("{\n" + (src == null ? "" : src) + "\n}");
      for (Statement st : block.getStatements()) {
        translateStatement(st, out, 0);
      }
    } catch (Exception e) {
      emit(out, 0, "Parse error (Java). Tipp: Snippet braucht gültige Statements (meist mit ;).");
      emit(out, 0, shortMessage(e));
    }

    return ensureTrailingNewline(out);
  }

  // ===========================
  // CLASS / INTERFACE / RECORD / ENUM
  // ===========================

  private String translateClassInternal(String src) {
    StringBuilder out = new StringBuilder();

    try {
      CompilationUnit cu = StaticJavaParser.parse(src);

      // reset enum cache for this run
      enumsByType.clear();
      enumTypesByConstant.clear();

      emitCompilationUnitHeaderComments(cu, out);

      // 1) Collect enums (top-level and nested) and emit ABAP interfaces for them
      collectEnums(cu);
      emitEnumInterfaces(out);

      // 2) Determine top-level primary type
      Optional<ClassOrInterfaceDeclaration> clsOpt = cu.findFirst(ClassOrInterfaceDeclaration.class);
      Optional<RecordDeclaration> recOpt = cu.findFirst(RecordDeclaration.class);
      Optional<EnumDeclaration> enumOnlyOpt = cu.findFirst(EnumDeclaration.class);

      // enum-only file (no class/record)
      if (clsOpt.isEmpty() && recOpt.isEmpty()) {
        if (enumOnlyOpt.isPresent()) {
          // We already emitted enum interface(s); that's the translation.
          return ensureTrailingNewline(out);
        }
        emit(out, 0, "Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/interface/record/enum.");
        return ensureTrailingNewline(out);
      }

      // INTERFACE
      if (clsOpt.isPresent() && clsOpt.get().isInterface()) {
        return ensureTrailingNewline(out.append(translateInterface(clsOpt.get())));
      }

      // RECORD
      if (recOpt.isPresent()) {
        return ensureTrailingNewline(out.append(translateRecord(recOpt.get())));
      }

      // CLASS
      ClassOrInterfaceDeclaration cls = clsOpt.get();
      out.append(translateClassDecl(cls));

    } catch (Exception e) {
      emit(out, 0, "Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/interface/record/enum.");
      emit(out, 0, shortMessage(e));
    }

    return ensureTrailingNewline(out);
  }

  private String translateInterface(ClassOrInterfaceDeclaration itf) {
    StringBuilder out = new StringBuilder();

    String javaName = itf.getNameAsString();
    String abapName = "zif_" + toSnakeLower(javaName);

    emitLeadingComments(itf, out, 0);

    out.append("INTERFACE ").append(abapName).append(" PUBLIC.\n");

    // interface fields -> constants (best-effort)
    for (FieldDeclaration fd : itf.getFields()) {
      for (VariableDeclarator v : fd.getVariables()) {
        if (v.getInitializer().isPresent() && isSimpleLiteral(v.getInitializer().get())) {
          emit(out, 2, "CONSTANTS " + v.getNameAsString()
              + " TYPE " + mapType(v.getTypeAsString())
              + " VALUE " + expr(v.getInitializer().get()) + ".");
        } else {
          hint(out, 2, "Interface field '" + v.getNameAsString() + "' not mapped (non-literal).");
        }
      }
    }

    // methods signatures
    for (MethodDeclaration md : itf.getMethods()) {
      emitLeadingComments(md, out, 2);
      out.append("  METHODS ").append(md.getNameAsString());

      if (!md.getParameters().isEmpty()) {
        out.append(" IMPORTING");
        for (int i = 0; i < md.getParameters().size(); i++) {
          Parameter p = md.getParameter(i);
          AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), true);
          out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(ti.abapType);
        }
      }

      if (!md.getType().isVoidType()) {
        AbapTypeInfo rti = mapTypeInfo(md.getTypeAsString(), "rv_result", true);
        out.append(" RETURNING VALUE(rv_result) TYPE ").append(rti.abapType);
      }

      out.append(".\n");
    }

    out.append("ENDINTERFACE.\n");
    return out.toString();
  }

  private String translateRecord(RecordDeclaration rec) {
    StringBuilder out = new StringBuilder();

    String javaName = rec.getNameAsString();
    String abapName = "zcl_" + toSnakeLower(javaName);

    emitLeadingComments(rec, out, 0);

    out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");
    out.append("  PUBLIC SECTION.\n");
    emit(out, 4, "\" record best-effort: components -> READ-ONLY attributes");

    for (Parameter p : rec.getParameters()) {
      AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), false);
      emitTypePreLines(out, 4, ti);
      emit(out, 4, "DATA " + p.getNameAsString() + " TYPE " + ti.abapType + " READ-ONLY.");
    }

    out.append("    METHODS constructor");
    if (!rec.getParameters().isEmpty()) {
      out.append(" IMPORTING");
      for (Parameter p : rec.getParameters()) {
        AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), true);
        out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(ti.abapType);
      }
    }
    out.append(".\n");
    out.append("ENDCLASS.\n\n");

    out.append("CLASS ").append(abapName).append(" IMPLEMENTATION.\n");
    out.append("  METHOD constructor.\n");
    for (Parameter p : rec.getParameters()) {
      emit(out, 2, "me->" + p.getNameAsString() + " = " + p.getNameAsString() + ".");
    }
    out.append("  ENDMETHOD.\n");
    out.append("ENDCLASS.\n");

    return out.toString();
  }

  private String translateClassDecl(ClassOrInterfaceDeclaration cls) {
    StringBuilder out = new StringBuilder();

    String javaName = cls.getNameAsString();
    String abapName = "zcl_" + toSnakeLower(javaName);

    emitLeadingComments(cls, out, 0);

    out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");

    Map<Section, List<FieldDeclaration>> fieldsBySec = new EnumMap<>(Section.class);
    Map<Section, List<CallableDeclaration<?>>> methodsBySec = new EnumMap<>(Section.class);
    for (Section s : Section.values()) {
      fieldsBySec.put(s, new ArrayList<>());
      methodsBySec.put(s, new ArrayList<>());
    }

    for (FieldDeclaration fd : cls.getFields()) {
      fieldsBySec.get(sectionOf(fd)).add(fd);
    }

    // constructors
    List<ConstructorDeclaration> ctors = cls.getConstructors();
    if (ctors.size() > 1) {
      hint(out, 0, "Multiple Java constructors detected; ABAP has only one CONSTRUCTOR (no overload).");
    }
    if (!ctors.isEmpty()) {
      methodsBySec.get(sectionOf(ctors.get(0))).add(ctors.get(0));
    }

    for (MethodDeclaration md : cls.getMethods()) {
      methodsBySec.get(sectionOf(md)).add(md);
    }

    emitSection(out, Section.PUBLIC, fieldsBySec, methodsBySec);
    emitSection(out, Section.PROTECTED, fieldsBySec, methodsBySec);
    emitSection(out, Section.PRIVATE, fieldsBySec, methodsBySec);

    out.append("ENDCLASS.\n\n");

    // ---------------------------
    // IMPLEMENTATION
    // ---------------------------
    out.append("CLASS ").append(abapName).append(" IMPLEMENTATION.\n");

    if (!ctors.isEmpty()) {
      out.append("  METHOD constructor.\n");
      for (Statement st : ctors.get(0).getBody().getStatements()) {
        translateStatement(st, out, 2);
      }
      out.append("  ENDMETHOD.\n\n");
    }

    for (MethodDeclaration md : cls.getMethods()) {
      out.append("  METHOD ").append(md.getNameAsString()).append(".\n");
      if (md.getBody().isPresent()) {
        for (Statement st : md.getBody().get().getStatements()) {
          translateStatement(st, out, 2);
        }
      } else {
        hint(out, 2, "Method body missing.");
      }
      out.append("  ENDMETHOD.\n\n");
    }

    out.append("ENDCLASS.\n");
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
    else return Section.PROTECTED; // safest

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
    out.append(switch (sec) {
      case PUBLIC -> "  PUBLIC SECTION.\n";
      case PROTECTED -> "  PROTECTED SECTION.\n";
      case PRIVATE -> "  PRIVATE SECTION.\n";
    });

    boolean any = false;

    for (FieldDeclaration fd : fieldsBySec.getOrDefault(sec, List.of())) {
      any = true;
      emitLeadingComments(fd, out, 4);
      for (VariableDeclarator v : fd.getVariables()) {
        emitDataDecl(out, 4, v.getNameAsString(), v.getTypeAsString(), v.getInitializer(), fd.isStatic(), fd.isFinal());
      }
    }

    for (CallableDeclaration<?> cd : methodsBySec.getOrDefault(sec, List.of())) {
      any = true;

      if (cd instanceof ConstructorDeclaration ctor) {
        emitLeadingComments(ctor, out, 4);
        out.append("    METHODS constructor");
        if (!ctor.getParameters().isEmpty()) {
          out.append(" IMPORTING");
          for (Parameter p : ctor.getParameters()) {
            AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), true);
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
            AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), true);
            out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(ti.abapType);
          }
        }

        if (!md.getType().isVoidType()) {
          AbapTypeInfo rti = mapTypeInfo(md.getTypeAsString(), "rv_result", true);
          out.append(" RETURNING VALUE(rv_result) TYPE ").append(rti.abapType);
        }

        out.append(".\n");
      }
    }

    if (!any) emit(out, 4, "\" (no members)");
  }

  // ===========================
  // ENUM SUPPORT (REAL OUTPUT)
  // ===========================

  private record AbapEnum(String javaName, String abapInterface, List<String> constants) {}

  private void collectEnums(CompilationUnit cu) {
    List<EnumDeclaration> enums = cu.findAll(EnumDeclaration.class);
    for (EnumDeclaration ed : enums) {
      String javaName = ed.getNameAsString();
      String abapItf = "zif_" + toSnakeLower(javaName);

      List<String> constants = ed.getEntries().stream()
          .map(EnumConstantDeclaration::getNameAsString)
          .toList();

      enumsByType.put(javaName, new AbapEnum(javaName, abapItf, constants));

      for (String c : constants) {
        enumTypesByConstant.computeIfAbsent(c, k -> new ArrayList<>()).add(javaName);
      }
    }
  }

  private void emitEnumInterfaces(StringBuilder out) {
    if (enumsByType.isEmpty()) return;

    // deterministic order
    List<AbapEnum> all = enumsByType.values().stream()
        .sorted(Comparator.comparing(a -> a.javaName))
        .toList();

    for (AbapEnum en : all) {
      emit(out, 0, "\" ---- enum " + en.javaName + " ----");
      emit(out, 0, "INTERFACE " + en.abapInterface + " PUBLIC.");
      emit(out, 2, "TYPES ty VALUE string."); // simple & usable for comparisons
      for (String c : en.constants) {
        emit(out, 2, "CONSTANTS c_" + toSnakeLower(c) + " TYPE ty VALUE '" + c + "'.");
      }
      emit(out, 0, "ENDINTERFACE.\n");
    }
  }

  private Optional<String> resolveEnumConstantQualified(String typeName, String constantName) {
    AbapEnum en = enumsByType.get(typeName);
    if (en == null) return Optional.empty();
    if (en.constants.stream().noneMatch(constantName::equals)) return Optional.empty();
    return Optional.of(en.abapInterface + "=>c_" + toSnakeLower(constantName));
  }

  private Optional<String> resolveEnumConstantUnqualified(String constantName) {
    List<String> types = enumTypesByConstant.getOrDefault(constantName, List.of());
    if (types.size() != 1) return Optional.empty(); // ambiguous or unknown
    return resolveEnumConstantQualified(types.get(0), constantName);
  }

  // ===========================
  // STATEMENTS
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
      ReturnStmt rs = st.asReturnStmt();
      rs.getExpression().ifPresent(x -> emit(out, indent, "rv_result = " + expr(x) + "."));
      emit(out, indent, "RETURN.");
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

    if (st.isForStmt()) {
      translateForStmt(st.asForStmt(), out, indent);
      return;
    }

    if (st.isForEachStmt()) {
      translateForEachStmt(st.asForEachStmt(), out, indent);
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
      hint(out, indent, "throw -> RAISE EXCEPTION TYPE <cx_...> (mapping unknown)");
      hint(out, indent, "thrown expr: " + expr(ts.getExpression()));
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

    hint(out, indent, "Unhandled statement: " + st.getClass().getSimpleName());
  }

  // ===========================
  // EXPRESSION STATEMENTS
  // ===========================

  private void translateExpressionStmt(ExpressionStmt es, StringBuilder out, int indent) {
    Expression e = es.getExpression();
    emitLeadingComments(e, out, indent);

    if (e.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = e.asVariableDeclarationExpr();
      for (VariableDeclarator v : vde.getVariables()) {
        emitDataDecl(out, indent, v.getNameAsString(), v.getTypeAsString(), Optional.empty(), false, false);

        if (v.getInitializer().isPresent()) {
          Expression init = v.getInitializer().get();

          if (init.isNullLiteralExpr()) {
            emit(out, indent, "CLEAR " + v.getNameAsString() + ".");
            continue;
          }

          if (init.isMethodCallExpr()) {
            MethodCallExpr mc = init.asMethodCallExpr();
            if (tryTranslateListGetIntoTarget(mc, v.getNameAsString(), out, indent)) continue;
            if (tryTranslateMapGetIntoTarget(mc, v.getNameAsString(), out, indent)) continue;
          }

          if (init.isObjectCreationExpr()) {
            // best-effort: CREATE OBJECT <var>.
            emit(out, indent, "CREATE OBJECT " + v.getNameAsString() + ".");
            if (!init.asObjectCreationExpr().getArguments().isEmpty()) {
              hint(out, indent, "constructor args need manual named mapping: "
                  + init.asObjectCreationExpr().getArguments().stream().map(this::expr).collect(Collectors.joining(", ")));
            }
            continue;
          }

          emit(out, indent, v.getNameAsString() + " = " + expr(init) + ".");
        }
      }
      return;
    }

    if (e.isAssignExpr()) {
      AssignExpr ae = e.asAssignExpr();
      String target = expr(ae.getTarget());

      if (ae.getOperator() == AssignExpr.Operator.ASSIGN && ae.getValue().isNullLiteralExpr()) {
        emit(out, indent, "CLEAR " + target + ".");
        return;
      }

      if (ae.getOperator() == AssignExpr.Operator.ASSIGN && ae.getValue().isMethodCallExpr()) {
        MethodCallExpr mc = ae.getValue().asMethodCallExpr();
        if (tryTranslateListGetIntoTarget(mc, target, out, indent)) return;
        if (tryTranslateMapGetIntoTarget(mc, target, out, indent)) return;
      }

      if (ae.getOperator() == AssignExpr.Operator.ASSIGN && ae.getValue().isObjectCreationExpr()) {
        emit(out, indent, "CREATE OBJECT " + target + ".");
        ObjectCreationExpr oce = ae.getValue().asObjectCreationExpr();
        if (!oce.getArguments().isEmpty()) {
          hint(out, indent, "constructor args need manual named mapping: "
              + oce.getArguments().stream().map(this::expr).collect(Collectors.joining(", ")));
        }
        return;
      }

      String value = expr(ae.getValue());

      switch (ae.getOperator()) {
        case ASSIGN -> emit(out, indent, target + " = " + value + ".");
        case PLUS -> emit(out, indent, target + " = " + target + " + " + value + ".");
        case MINUS -> emit(out, indent, target + " = " + target + " - " + value + ".");
        case MULTIPLY -> emit(out, indent, target + " = " + target + " * " + value + ".");
        case DIVIDE -> emit(out, indent, target + " = " + target + " / " + value + ".");
        default -> {
          hint(out, indent, "compound assignment not mapped: " + ae.getOperator());
          emit(out, indent, target + " = " + value + ".");
        }
      }
      return;
    }

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

      // List: add / clear
      if (mc.getScope().isPresent() && mc.getNameAsString().equals("add") && mc.getArguments().size() == 1) {
        emit(out, indent, "APPEND " + expr(mc.getArgument(0)) + " TO " + expr(mc.getScope().get()) + ".");
        return;
      }
      if (mc.getScope().isPresent() && mc.getNameAsString().equals("clear") && mc.getArguments().isEmpty()) {
        emit(out, indent, "CLEAR " + expr(mc.getScope().get()) + ".");
        return;
      }

      // Map: put
      if (mc.getScope().isPresent() && mc.getNameAsString().equals("put") && mc.getArguments().size() == 2) {
        String map = expr(mc.getScope().get());
        String k = expr(mc.getArgument(0));
        String v = expr(mc.getArgument(1));
        emit(out, indent, "INSERT VALUE #( key = " + k + " value = " + v + " ) INTO TABLE " + map + ".");
        return;
      }

      // generic ABAP OO call
      emit(out, indent, abapMethodCallStatement(mc));
      return;
    }

    hint(out, indent, "Unhandled expression stmt: " + e.getClass().getSimpleName());
  }

  // ===========================
  // LIST.GET / MAP.GET (as statements)
  // ===========================

  private boolean tryTranslateListGetIntoTarget(MethodCallExpr mc, String target, StringBuilder out, int indent) {
    if (mc == null) return false;
    if (!mc.getNameAsString().equals("get")) return false;
    if (mc.getScope().isEmpty()) return false;
    if (mc.getArguments().size() != 1) return false;

    String list = expr(mc.getScope().get());
    Expression idxExpr = mc.getArgument(0);

    // 0-based Java -> 1-based ABAP INDEX
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
    String selector = expr(ss.getSelector());
    emit(out, indent, "CASE " + selector + ".");

    List<SwitchEntry> entries = ss.getEntries();
    int i = 0;
    while (i < entries.size()) {
      SwitchEntry entry = entries.get(i);

      List<String> labels = new ArrayList<>();
      boolean isDefault = entry.getLabels().isEmpty();
      if (!isDefault) labels.addAll(entry.getLabels().stream().map(this::switchLabelExpr).toList());

      List<Statement> stmts = new ArrayList<>(entry.getStatements());
      int j = i;
      while (stmts.isEmpty() && j + 1 < entries.size()) {
        SwitchEntry next = entries.get(j + 1);
        if (!next.getLabels().isEmpty()) labels.addAll(next.getLabels().stream().map(this::switchLabelExpr).toList());
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

    // FRIDAY -> if unambiguous enum constant: zif_day=>c_friday
    if (e.isNameExpr()) {
      String c = e.asNameExpr().getNameAsString();
      Optional<String> mapped = resolveEnumConstantUnqualified(c);
      if (mapped.isPresent()) return mapped.get();
      return "'" + c + "'";
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

    hint(out, indent, "for-loop not mapped (non-trivial).");
    translateStatement(fs.getBody(), out, indent);
  }

  private void translateForEachStmt(ForEachStmt fes, StringBuilder out, int indent) {
    String var = fes.getVariable().getVariable(0).getNameAsString();
    String iterable = expr(fes.getIterable());

    // if "var": LOOP ... INTO DATA(var)
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
  // COLLECTION TYPE MAPPING
  // ===========================

  private record AbapTypeInfo(String abapType, List<String> preLines) {}

  private record Generic(String base, List<String> args) {}

  private AbapTypeInfo mapTypeInfo(String javaTypeRaw, String varName, boolean forSignature) {
    String t = javaTypeRaw == null ? "" : javaTypeRaw.trim();

    // arrays
    if (t.endsWith("[]")) {
      String elem = t.substring(0, t.length() - 2).trim();
      String ab = mapType(elem);
      String tt = forSignature ? ("STANDARD TABLE OF " + ab) : ("STANDARD TABLE OF " + ab + " WITH EMPTY KEY");
      return new AbapTypeInfo(tt, List.of());
    }

    Optional<Generic> g = parseGeneric(t);
    if (g.isPresent()) {
      Generic gg = g.get();

      // List<T>
      if (isListBase(gg.base)) {
        String elemJava = gg.args.isEmpty() ? "String" : gg.args.get(0);
        String elemAbap = mapType(elemJava);
        if ("String".equals(elemJava)) return new AbapTypeInfo("string_table", List.of());
        String tt = forSignature ? ("STANDARD TABLE OF " + elemAbap) : ("STANDARD TABLE OF " + elemAbap + " WITH EMPTY KEY");
        return new AbapTypeInfo(tt, List.of());
      }

      // Map<K,V> -> hashed table of entry type
      if (isMapBase(gg.base)) {
        String keyJava = gg.args.size() > 0 ? gg.args.get(0) : "String";
        String valJava = gg.args.size() > 1 ? gg.args.get(1) : "String";
        String keyAbap = mapType(keyJava);
        String valAbap = mapType(valJava);

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

    return new AbapTypeInfo(mapType(t), List.of());
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
    String b = base.trim();
    return b.equals("List") || b.equals("ArrayList") || b.equals("LinkedList") || b.equals("Collection");
  }

  private boolean isMapBase(String base) {
    if (base == null) return false;
    String b = base.trim();
    return b.equals("Map") || b.equals("HashMap") || b.equals("LinkedHashMap") || b.equals("TreeMap");
  }

  private void emitTypePreLines(StringBuilder out, int indent, AbapTypeInfo ti) {
    for (String line : ti.preLines) emit(out, indent, line);
  }

  private void emitDataDecl(
      StringBuilder out,
      int indent,
      String name,
      String javaType,
      Optional<Expression> initializer,
      boolean isStatic,
      boolean isFinal
  ) {
    AbapTypeInfo ti = mapTypeInfo(javaType, name, false);
    emitTypePreLines(out, indent, ti);

    if (isStatic && isFinal && initializer.isPresent() && isSimpleLiteral(initializer.get())) {
      emit(out, indent, "CONSTANTS " + name + " TYPE " + mapType(javaType) + " VALUE " + expr(initializer.get()) + ".");
      return;
    }

    emit(out, indent, (isStatic ? "CLASS-DATA " : "DATA ") + name + " TYPE " + ti.abapType + ".");

    if (initializer.isPresent() && !initializer.get().isNullLiteralExpr() && !initializer.get().isObjectCreationExpr()) {
      emit(out, indent, name + " = " + expr(initializer.get()) + ".");
    }
  }

  private boolean isSimpleLiteral(Expression e) {
    if (e == null) return false;
    return e.isIntegerLiteralExpr() || e.isBooleanLiteralExpr() || e.isStringLiteralExpr()
        || e.isCharLiteralExpr() || e.isDoubleLiteralExpr() || e.isLongLiteralExpr();
  }

  // ===========================
  // METHOD CALL STATEMENT
  // ===========================

  private String abapMethodCallStatement(MethodCallExpr mc) {
    String callTarget;
    if (mc.getScope().isPresent()) {
      callTarget = abapCallTarget(mc.getScope().get()) + mc.getNameAsString();
    } else {
      callTarget = "me->" + mc.getNameAsString();
    }

    if (mc.getArguments().isEmpty()) return callTarget + "( ).";

    // More usable than "TODO": emit EXPORTING iv_1/iv_2 placeholders
    StringBuilder b = new StringBuilder();
    b.append("CALL METHOD ").append(callTarget).append("\n");
    b.append("  EXPORTING\n");
    for (int i = 0; i < mc.getArguments().size(); i++) {
      b.append("    iv_").append(i + 1).append(" = ").append(expr(mc.getArgument(i))).append("\n");
    }
    b.append("."); // ABAP statement end
    return b.toString();
  }

  private String abapCallTarget(Expression scope) {
    if (scope == null) return "me->";

    if (scope.isThisExpr()) return "me->";
    if (scope.isSuperExpr()) return "super->";

    if (scope.isNameExpr()) {
      String n = scope.asNameExpr().getNameAsString();

      // static call: ClassName.method()
      if (looksLikeTypeName(n)) return "zcl_" + toSnakeLower(n) + "=>";
      return n + "->";
    }

    if (scope.isFieldAccessExpr()) {
      // this.foo.bar -> me->foo->bar
      String base = access(scope.asFieldAccessExpr());
      if (base.endsWith("=>")) return base;
      return base + "->";
    }

    return expr(scope) + "->";
  }

  // ===========================
  // EXPR
  // ===========================

  private String expr(Expression e) {
    if (e == null) return "''";

    if (e.isThisExpr()) return "me";
    if (e.isSuperExpr()) return "super";
    if (e.isNullLiteralExpr()) return "INITIAL";

    if (e.isNameExpr()) {
      String n = e.asNameExpr().getNameAsString();

      // unqualified enum constant if unambiguous
      Optional<String> ec = resolveEnumConstantUnqualified(n);
      if (ec.isPresent()) return ec.get();

      return n;
    }

    if (e.isFieldAccessExpr()) return access(e.asFieldAccessExpr());

    if (e.isIntegerLiteralExpr()) return e.asIntegerLiteralExpr().getValue();
    if (e.isLongLiteralExpr()) return e.asLongLiteralExpr().getValue();
    if (e.isDoubleLiteralExpr()) return e.asDoubleLiteralExpr().getValue();
    if (e.isBooleanLiteralExpr()) return e.asBooleanLiteralExpr().getValue() ? "abap_true" : "abap_false";
    if (e.isStringLiteralExpr()) return "'" + e.asStringLiteralExpr().asString().replace("'", "''") + "'";
    if (e.isCharLiteralExpr()) return "'" + e.asCharLiteralExpr().asChar() + "'";
    if (e.isEnclosedExpr()) return "(" + expr(e.asEnclosedExpr().getInner()) + ")";

    if (e.isBinaryExpr()) {
      BinaryExpr b = e.asBinaryExpr();

      // null comparisons -> INITIAL / BOUND best-effort
      if ((b.getOperator() == BinaryExpr.Operator.EQUALS || b.getOperator() == BinaryExpr.Operator.NOT_EQUALS) &&
          (b.getLeft().isNullLiteralExpr() || b.getRight().isNullLiteralExpr())) {
        Expression other = b.getLeft().isNullLiteralExpr() ? b.getRight() : b.getLeft();
        String opnd = expr(other);
        boolean notEq = b.getOperator() == BinaryExpr.Operator.NOT_EQUALS;

        // We cannot know ref-vs-nonref reliably; use INITIAL (works for most)
        return notEq ? (opnd + " IS NOT INITIAL") : (opnd + " IS INITIAL");
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
      if (u.getOperator() == UnaryExpr.Operator.LOGICAL_COMPLEMENT) return "NOT (" + expr(u.getExpression()) + ")";
      return expr(u.getExpression());
    }

    if (e.isConditionalExpr()) {
      return "COND #( WHEN " + expr(e.asConditionalExpr().getCondition()) +
          " THEN " + expr(e.asConditionalExpr().getThenExpr()) +
          " ELSE " + expr(e.asConditionalExpr().getElseExpr()) + " )";
    }

    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();
      if (isSystemOutPrintln(mc) || isSystemOutPrint(mc)) return "''";

      // list.size() / isEmpty() in expressions
      if (mc.getScope().isPresent() && mc.getArguments().isEmpty() && mc.getNameAsString().equals("size")) {
        return "lines( " + expr(mc.getScope().get()) + " )";
      }
      if (mc.getScope().isPresent() && mc.getArguments().isEmpty() && mc.getNameAsString().equals("isEmpty")) {
        return "lines( " + expr(mc.getScope().get()) + " ) = 0";
      }

      // list.get / map.get cannot be inlined cleanly -> leave marker
      if (mc.getScope().isPresent() && mc.getNameAsString().equals("get") && mc.getArguments().size() == 1) {
        return "(* READ TABLE needed *)";
      }

      return mc.getNameAsString() + "( ... )";
    }

    return "''";
  }

  /**
   * Field access:
   * - this.x        -> me->x
   * - this.x.y      -> me->x->y
   * - super.x       -> super->x
   * - obj.x         -> obj->x
   * - Day.FRIDAY    -> zif_day=>c_friday   (enum)
   * - Class.CONST   -> zcl_class=>const    (best-effort static)
   */
  private String access(FieldAccessExpr fa) {
    Expression scope = fa.getScope();
    String name = fa.getNameAsString();

    if (scope.isThisExpr()) return "me->" + name;
    if (scope.isSuperExpr()) return "super->" + name;

    // Enum constant: Day.FRIDAY
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
  // COMMENTS
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
  // System.out
  // ===========================

  private boolean isSystemOutPrintln(MethodCallExpr mc) {
    if (!mc.getNameAsString().equals("println")) return false;
    if (mc.getScope().isEmpty()) return false;
    Expression scope = mc.getScope().get();
    if (!scope.isFieldAccessExpr()) return false;
    FieldAccessExpr fa = scope.asFieldAccessExpr();
    return fa.getNameAsString().equals("out")
        && fa.getScope().isNameExpr()
        && fa.getScope().asNameExpr().getNameAsString().equals("System");
  }

  private boolean isSystemOutPrint(MethodCallExpr mc) {
    if (!mc.getNameAsString().equals("print")) return false;
    if (mc.getScope().isEmpty()) return false;
    Expression scope = mc.getScope().get();
    if (!scope.isFieldAccessExpr()) return false;
    FieldAccessExpr fa = scope.asFieldAccessExpr();
    return fa.getNameAsString().equals("out")
        && fa.getScope().isNameExpr()
        && fa.getScope().asNameExpr().getNameAsString().equals("System");
  }

  // ===========================
  // Helpers
  // ===========================

  private static void emit(StringBuilder out, int indent, String line) {
    out.append(" ".repeat(Math.max(0, indent))).append(line).append("\n");
  }

  private static void hint(StringBuilder out, int indent, String msg) {
    if (!EMIT_HINTS) return;
    emit(out, indent, "\" TODO: " + msg);
  }

  private String ensureTrailingNewline(StringBuilder out) {
    String s = out.toString().replace("\r\n", "\n").replace("\r", "\n").trim();
    return s.isEmpty() ? "\n" : (s + "\n");
  }

  private boolean looksLikeClass(String src) {
    String s = src == null ? "" : src;
    return s.contains("class ") || s.contains("interface ") || s.contains("enum ") || s.contains("record ")
        || s.startsWith("package ") || s.startsWith("import ");
  }

  private String mapType(String javaType) {
    String t = javaType == null ? "" : javaType.trim();
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
