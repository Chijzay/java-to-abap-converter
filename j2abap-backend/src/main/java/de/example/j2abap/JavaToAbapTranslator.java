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

  // ---------------------------
  // State (per translation)
  // ---------------------------
  private final Map<String, List<String>> enumConstantsByType = new LinkedHashMap<>();
  private final Map<String, List<String>> enumTypesByConstant = new HashMap<>();
  private final Set<String> declaredTemps = new HashSet<>();

  // ---------------------------
  // Public API
  // ---------------------------
  public String translateAuto(String javaCode) {
    String src = normalize(javaCode);
    if (src.isBlank()) return "\n";

    // AUTO: prefer real class translation if we can parse a CompilationUnit with at least one type
    ParseAttempt a = tryParseCompilationUnit(src);
    if (a.ok && a.cu != null && !a.cu.getTypes().isEmpty()) {
      return translateClassInternal(a.fixedSource);
    }

    // otherwise snippet
    return translateSnippetInternal(src);
  }

  public String translateSnippet(String javaCode) {
    return translateSnippetInternal(normalize(javaCode));
  }

  public String translateClass(String javaCode) {
    return translateClassInternal(normalize(javaCode));
  }

  public String translate(String javaCode, String mode) {
    String src = normalize(javaCode);
    String m = (mode == null ? "auto" : mode.trim().toLowerCase(Locale.ROOT));
    return switch (m) {
      case "class" -> translateClass(src);
      case "snippet" -> translateSnippet(src);
      default -> translateAuto(src);
    };
  }

  // ===========================
  // SNIPPET
  // - no CLASS/REPORT wrappers
  // - supports "mixed": enum/class + free statements
  // ===========================
  private String translateSnippetInternal(String src) {
    resetState();
    StringBuilder out = new StringBuilder();

    Extracted ex = extractTopLevelTypeDeclsAnywhere(src);

    // If user selected snippet but provided only types -> don't produce empty output
    if ((ex.remainingCode == null || ex.remainingCode.isBlank())
        && ex.extractedTypes != null && !ex.extractedTypes.isBlank()) {
      emit(out, 0, "\" Hinweis: „Snippet“ gewählt, aber dein Input sieht nach kompletter Klasse/Enum aus.");
      emit(out, 0, "\" Bitte „Class“ wählen oder nur Statements einfügen.");
      return ensureTrailingNewline(out);
    }

    String wrapper =
        "class __J2ABAP {\n" +
            ex.extractedTypes +
            "  void __m() {\n" +
            ex.remainingCode + "\n" +
            "  }\n" +
            "}\n";

    try {
      CompilationUnit cu = StaticJavaParser.parse(wrapper);

      indexEnums(cu);
      inferEnumsFromSwitches(cu);
      emitEnumDeclarationsSnippet(out);

      MethodDeclaration m = cu.findFirst(MethodDeclaration.class, md -> md.getNameAsString().equals("__m"))
          .orElse(null);

      if (m == null || m.getBody().isEmpty()) {
        emit(out, 0, "\" TODO: no statements found");
        return ensureTrailingNewline(out);
      }

      for (Statement st : m.getBody().get().getStatements()) {
        translateStatement(st, out, 0);
      }

    } catch (Exception e) {
      out.append("Parse error (Java). Tipp: Snippet braucht gültige Statements.\n");
      out.append(shortMessage(e)).append("\n");
    }

    return ensureTrailingNewline(out);
  }

  // ===========================
  // CLASS
  // - outputs CLASS DEFINITION + IMPLEMENTATION
  // - robust parse (brace auto-fix)
  // ===========================
  private String translateClassInternal(String src) {
    resetState();
    StringBuilder out = new StringBuilder();

    ParseAttempt a = tryParseCompilationUnit(src);
    if (!a.ok || a.cu == null) {
      out.append("Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/interface/record/enum.\n");
      out.append(a.errorMessage == null ? "" : a.errorMessage).append("\n");
      return ensureTrailingNewline(out);
    }

    CompilationUnit cu = a.cu;

    emitCompilationUnitHeaderComments(cu, out);

    indexEnums(cu);
    inferEnumsFromSwitches(cu);

    // If only enums exist as top-level, emit snippet-like enum constants (still useful)
    if (cu.getTypes().isEmpty() && !enumConstantsByType.isEmpty()) {
      emitEnumDeclarationsSnippet(out);
      return ensureTrailingNewline(out);
    }

    // Prefer top-level class/interface; fall back to first type
    Optional<ClassOrInterfaceDeclaration> clsOpt = cu.findFirst(ClassOrInterfaceDeclaration.class);
    Optional<RecordDeclaration> recOpt = cu.findFirst(RecordDeclaration.class);

    if (recOpt.isPresent() && clsOpt.isEmpty()) {
      return translateRecordAsClass(recOpt.get(), out);
    }

    if (clsOpt.isEmpty()) {
      out.append("Parse error (Java). Tipp: Keine class/interface gefunden.\n");
      return ensureTrailingNewline(out);
    }

    ClassOrInterfaceDeclaration cls = clsOpt.get();

    if (cls.isInterface()) {
      return translateInterface(cls, out);
    }

    return translateClass(cls, out);
  }

  private String translateInterface(ClassOrInterfaceDeclaration itf, StringBuilder out) {
    String abapName = "zif_" + toSnakeLower(itf.getNameAsString());
    emitLeadingComments(itf, out, 0);

    out.append("INTERFACE ").append(abapName).append(" PUBLIC.\n");

    for (MethodDeclaration md : itf.getMethods()) {
      emitLeadingComments(md, out, 2);

      out.append("  METHODS ").append(md.getNameAsString());

      if (!md.getParameters().isEmpty()) {
        out.append(" IMPORTING");
        for (Parameter p : md.getParameters()) {
          out.append(" ").append(p.getNameAsString())
              .append(" TYPE ").append(mapTypeForAbap(p.getTypeAsString()));
        }
      }

      if (!md.getType().isVoidType()) {
        out.append(" RETURNING VALUE(rv_result) TYPE ").append(mapTypeForAbap(md.getTypeAsString()));
      }

      out.append(".\n");
    }

    out.append("ENDINTERFACE.\n");
    return ensureTrailingNewline(out);
  }

  private String translateRecordAsClass(RecordDeclaration rec, StringBuilder out) {
    String abapName = "zcl_" + toSnakeLower(rec.getNameAsString());
    emitLeadingComments(rec, out, 0);

    out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");
    out.append("  PUBLIC SECTION.\n");

    emitEnumDeclarationsInClassPublic(out);

    emit(out, 4, "\" record mapped best-effort: components -> READ-ONLY attributes");
    for (Parameter p : rec.getParameters()) {
      emit(out, 4, "DATA " + p.getNameAsString() + " TYPE " + mapTypeForAbap(p.getTypeAsString()) + " READ-ONLY.");
    }

    out.append("    METHODS constructor");
    if (!rec.getParameters().isEmpty()) {
      out.append(" IMPORTING");
      for (Parameter p : rec.getParameters()) {
        out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(mapTypeForAbap(p.getTypeAsString()));
      }
    }
    out.append(".\n");

    out.append("ENDCLASS.\n\n");

    out.append("CLASS ").append(abapName).append(" IMPLEMENTATION.\n");
    out.append("  METHOD constructor.\n");
    for (Parameter p : rec.getParameters()) {
      emit(out, 4, "me->" + p.getNameAsString() + " = " + p.getNameAsString() + ".");
    }
    out.append("  ENDMETHOD.\n");
    out.append("ENDCLASS.\n");

    return ensureTrailingNewline(out);
  }

  private String translateClass(ClassOrInterfaceDeclaration cls, StringBuilder out) {
    String abapName = "zcl_" + toSnakeLower(cls.getNameAsString());
    emitLeadingComments(cls, out, 0);

    out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");
    out.append("  PUBLIC SECTION.\n");

    // Enums (best-effort) as TYPES + CONSTANTS
    emitEnumDeclarationsInClassPublic(out);

    // Fields (treat package-private as PUBLIC for usability)
    for (FieldDeclaration fd : cls.getFields()) {
      emitLeadingComments(fd, out, 4);

      for (VariableDeclarator v : fd.getVariables()) {
        String name = v.getNameAsString();
        String javaType = v.getTypeAsString();

        boolean constCandidate = fd.isStatic() && fd.isFinal()
            && v.getInitializer().isPresent()
            && isSimpleLiteral(v.getInitializer().get());

        if (constCandidate) {
          emit(out, 4, "CONSTANTS " + name + " TYPE " + mapTypeForAbap(javaType)
              + " VALUE " + expr(v.getInitializer().get()) + ".");
          continue;
        }

        String kw = fd.isStatic() ? "CLASS-DATA" : "DATA";
        emit(out, 4, kw + " " + name + " TYPE " + mapTypeForAbap(javaType) + ".");
      }
    }

    // Constructors: ABAP has only one constructor -> take first
    List<ConstructorDeclaration> ctors = cls.getConstructors();
    if (!ctors.isEmpty()) {
      ConstructorDeclaration cd = ctors.get(0);
      out.append("    METHODS constructor");
      if (!cd.getParameters().isEmpty()) {
        out.append(" IMPORTING");
        for (Parameter p : cd.getParameters()) {
          out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(mapTypeForAbap(p.getTypeAsString()));
        }
      }
      out.append(".\n");
    }

    // Methods
    for (MethodDeclaration md : cls.getMethods()) {
      emitLeadingComments(md, out, 4);

      String kw = md.isStatic() ? "CLASS-METHODS " : "METHODS ";
      out.append("    ").append(kw).append(md.getNameAsString());

      if (!md.getParameters().isEmpty()) {
        out.append(" IMPORTING");
        for (Parameter p : md.getParameters()) {
          out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(mapTypeForAbap(p.getTypeAsString()));
        }
      }

      if (!md.getType().isVoidType()) {
        out.append(" RETURNING VALUE(rv_result) TYPE ").append(mapTypeForAbap(md.getTypeAsString()));
      }

      out.append(".\n");
    }

    out.append("ENDCLASS.\n\n");

    // IMPLEMENTATION
    out.append("CLASS ").append(abapName).append(" IMPLEMENTATION.\n");

    // Constructor body
    if (!ctors.isEmpty()) {
      ConstructorDeclaration cd = ctors.get(0);
      out.append("  METHOD constructor.\n");
      for (Statement st : cd.getBody().getStatements()) {
        translateStatement(st, out, 4);
      }
      out.append("  ENDMETHOD.\n\n");

      if (ctors.size() > 1) {
        emit(out, 2, "\" TODO: multiple Java constructors exist; ABAP has no overload (only first translated).");
        out.append("\n");
      }
    }

    // Methods bodies
    for (MethodDeclaration md : cls.getMethods()) {
      out.append("  METHOD ").append(md.getNameAsString()).append(".\n");

      if (md.getBody().isPresent()) {
        for (Statement st : md.getBody().get().getStatements()) {
          translateStatement(st, out, 4);
        }
      } else {
        emit(out, 4, "\" TODO: method body missing");
      }

      out.append("  ENDMETHOD.\n\n");
    }

    out.append("ENDCLASS.\n");
    return ensureTrailingNewline(out);
  }

  // ===========================
  // Enum collection + inference
  // ===========================
  private void indexEnums(CompilationUnit cu) {
    enumConstantsByType.clear();
    enumTypesByConstant.clear();

    for (EnumDeclaration ed : cu.findAll(EnumDeclaration.class)) {
      String type = ed.getNameAsString();
      List<String> constants = ed.getEntries().stream()
          .map(EnumConstantDeclaration::getNameAsString)
          .toList();
      enumConstantsByType.put(type, constants);
      for (String c : constants) {
        enumTypesByConstant.computeIfAbsent(c, k -> new ArrayList<>()).add(type);
      }
    }
  }

  /**
   * If enum isn't declared but switch(day) uses case MONDAY/FRIDAY... and `day` is typed as Day,
   * we infer enum Day = [MONDAY, FRIDAY, ...] so that case labels can be translated.
   */
  private void inferEnumsFromSwitches(CompilationUnit cu) {
    for (SwitchStmt ss : cu.findAll(SwitchStmt.class)) {
      if (!ss.getSelector().isNameExpr()) continue;

      String varName = ss.getSelector().asNameExpr().getNameAsString();
      String typeName = resolveVarTypeName(ss, varName);
      if (typeName == null) continue;

      typeName = stripGeneric(typeName);
      if (!looksLikeTypeName(typeName)) continue;
      if (enumConstantsByType.containsKey(typeName)) continue;

      List<String> labels = ss.getEntries().stream()
          .flatMap(e -> e.getLabels().stream())
          .filter(Expression::isNameExpr)
          .map(e -> e.asNameExpr().getNameAsString())
          .distinct()
          .toList();

      if (labels.isEmpty()) continue;

      enumConstantsByType.put(typeName, labels);
      for (String c : labels) {
        enumTypesByConstant.computeIfAbsent(c, k -> new ArrayList<>()).add(typeName);
      }
    }
  }

  private String resolveVarTypeName(com.github.javaparser.ast.Node node, String varName) {
    // method params + locals
    Optional<MethodDeclaration> md = node.findAncestor(MethodDeclaration.class);
    if (md.isPresent()) {
      for (Parameter p : md.get().getParameters()) {
        if (p.getNameAsString().equals(varName)) return p.getTypeAsString();
      }
      for (VariableDeclarator vd : md.get().findAll(VariableDeclarator.class)) {
        if (vd.getNameAsString().equals(varName)) return vd.getTypeAsString();
      }
    }

    // class fields
    Optional<ClassOrInterfaceDeclaration> cd = node.findAncestor(ClassOrInterfaceDeclaration.class);
    if (cd.isPresent()) {
      for (FieldDeclaration fd : cd.get().getFields()) {
        for (VariableDeclarator vd : fd.getVariables()) {
          if (vd.getNameAsString().equals(varName)) return vd.getTypeAsString();
        }
      }
    }
    return null;
  }

  private void emitEnumDeclarationsSnippet(StringBuilder out) {
    if (enumConstantsByType.isEmpty()) return;

    for (Map.Entry<String, List<String>> e : enumConstantsByType.entrySet()) {
      String enumType = e.getKey();
      String ty = "ty_" + toSnakeLower(enumType);

      emit(out, 0, "TYPES " + ty + " TYPE string.");
      for (String c : e.getValue()) {
        emit(out, 0, "CONSTANTS c_" + toSnakeLower(enumType) + "_" + toSnakeLower(c)
            + " TYPE " + ty + " VALUE '" + c + "'.");
      }
      emit(out, 0, "");
    }
  }

  private void emitEnumDeclarationsInClassPublic(StringBuilder out) {
    if (enumConstantsByType.isEmpty()) return;

    emit(out, 4, "\" --- enums (best-effort) ---");
    for (Map.Entry<String, List<String>> e : enumConstantsByType.entrySet()) {
      String enumType = e.getKey();
      String ty = "ty_" + toSnakeLower(enumType);
      emit(out, 4, "TYPES " + ty + " TYPE string.");
      for (String c : e.getValue()) {
        emit(out, 4, "CONSTANTS c_" + toSnakeLower(enumType) + "_" + toSnakeLower(c)
            + " TYPE " + ty + " VALUE '" + c + "'.");
      }
      emit(out, 4, "");
    }
  }

  private Optional<String> resolveEnumConstantQualified(String enumType, String constant) {
    List<String> cs = enumConstantsByType.get(enumType);
    if (cs == null) return Optional.empty();
    if (!cs.contains(constant)) return Optional.empty();
    return Optional.of("c_" + toSnakeLower(enumType) + "_" + toSnakeLower(constant));
  }

  private Optional<String> resolveEnumConstantUnqualified(String constant) {
    List<String> types = enumTypesByConstant.getOrDefault(constant, List.of());
    if (types.size() != 1) return Optional.empty();
    return resolveEnumConstantQualified(types.get(0), constant);
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

    if (st.isSwitchStmt()) {
      translateSwitchStmt(st.asSwitchStmt(), out, indent);
      return;
    }

    if (st.isTryStmt()) {
      translateTryStmt(st.asTryStmt(), out, indent);
      return;
    }

    if (st.isWhileStmt()) {
      WhileStmt ws = st.asWhileStmt();
      emit(out, indent, "WHILE " + expr(ws.getCondition()) + ".");
      translateStatement(ws.getBody(), out, indent + 2);
      emit(out, indent, "ENDWHILE.");
      return;
    }

    if (st.isForEachStmt()) {
      ForEachStmt fes = st.asForEachStmt();
      String var = fes.getVariable().getVariable(0).getNameAsString();
      String it = expr(fes.getIterable());
      emit(out, indent, "LOOP AT " + it + " INTO " + var + ".");
      translateStatement(fes.getBody(), out, indent + 2);
      emit(out, indent, "ENDLOOP.");
      return;
    }

    if (st.isReturnStmt()) {
      ReturnStmt rs = st.asReturnStmt();
      if (rs.getExpression().isPresent()) {
        emit(out, indent, "rv_result = " + expr(rs.getExpression().get()) + ".");
      }
      emit(out, indent, "RETURN.");
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

    emit(out, indent, "\" TODO: statement not mapped: " + prettyNodeName(st.getClass().getSimpleName()));
  }

  private void translateTryStmt(TryStmt ts, StringBuilder out, int indent) {
    emit(out, indent, "TRY.");
    for (Statement st : ts.getTryBlock().getStatements()) translateStatement(st, out, indent + 2);

    for (CatchClause cc : ts.getCatchClauses()) {
      String var = cc.getParameter().getNameAsString();
      emit(out, indent, "CATCH cx_root INTO DATA(" + var + ").");
      emit(out, indent + 2, "\" TODO: Java exception type not mapped: " + cc.getParameter().getTypeAsString());
      for (Statement st : cc.getBody().getStatements()) translateStatement(st, out, indent + 2);
    }

    if (ts.getFinallyBlock().isPresent()) {
      emit(out, indent, "FINALLY.");
      for (Statement st : ts.getFinallyBlock().get().getStatements()) translateStatement(st, out, indent + 2);
    }

    emit(out, indent, "ENDTRY.");
  }

  private void translateSwitchStmt(SwitchStmt ss, StringBuilder out, int indent) {
    // Group fall-through labels:
    // case A: (no stmts) case B: (stmts) => WHEN A OR B.
    emit(out, indent, "CASE " + expr(ss.getSelector()) + ".");

    List<SwitchEntry> entries = ss.getEntries();
    int i = 0;
    while (i < entries.size()) {
      SwitchEntry entry = entries.get(i);

      boolean isDefault = entry.getLabels().isEmpty();
      List<String> labels = new ArrayList<>();
      if (!isDefault) labels.addAll(entry.getLabels().stream().map(this::switchLabelExpr).toList());

      List<Statement> stmts = new ArrayList<>(entry.getStatements());
      int j = i;
      while (stmts.isEmpty() && j + 1 < entries.size()) {
        SwitchEntry next = entries.get(j + 1);
        if (next.getLabels().isEmpty()) {
          isDefault = true;
        } else {
          labels.addAll(next.getLabels().stream().map(this::switchLabelExpr).toList());
        }
        stmts = new ArrayList<>(next.getStatements());
        j++;
        if (!stmts.isEmpty()) break;
      }

      if (isDefault || labels.isEmpty()) {
        emit(out, indent, "  WHEN OTHERS.");
      } else {
        emit(out, indent, "  WHEN " + String.join(" OR ", labels) + ".");
      }

      for (Statement st : stmts) {
        if (st.isBreakStmt()) continue; // ABAP CASE doesn't need break
        translateStatement(st, out, indent + 4);
      }

      i = j + 1;
    }

    emit(out, indent, "ENDCASE.");
  }

  private String switchLabelExpr(Expression e) {
    if (e == null) return "''";
    if (e.isNameExpr()) {
      String n = e.asNameExpr().getNameAsString();
      return resolveEnumConstantUnqualified(n).orElse(n);
    }
    if (e.isFieldAccessExpr()) {
      FieldAccessExpr fa = e.asFieldAccessExpr();
      if (fa.getScope().isNameExpr()) {
        return resolveEnumConstantQualified(fa.getScope().asNameExpr().getNameAsString(), fa.getNameAsString())
            .orElse(accessExpr(fa));
      }
      return accessExpr(fa);
    }
    return expr(e);
  }

  // ===========================
  // Expression statements
  // ===========================
  private void translateExpressionStmt(ExpressionStmt es, StringBuilder out, int indent) {
    Expression e = es.getExpression();

    emitLeadingComments(e, out, indent);

    if (e.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = e.asVariableDeclarationExpr();
      for (VariableDeclarator v : vde.getVariables()) {
        String name = v.getNameAsString();
        String javaType = v.getTypeAsString();

        if (v.getInitializer().isPresent() && v.getInitializer().get().isObjectCreationExpr()) {
          ObjectCreationExpr oce = v.getInitializer().get().asObjectCreationExpr();
          emitNewVar(name, javaType, oce, out, indent);
        } else {
          emit(out, indent, "DATA " + name + " TYPE " + mapTypeForAbap(javaType) + ".");
          v.getInitializer().ifPresent(init -> emit(out, indent, name + " = " + expr(init) + "."));
        }
      }
      return;
    }

    if (e.isAssignExpr()) {
      AssignExpr ae = e.asAssignExpr();
      String target = expr(ae.getTarget());
      Expression val = ae.getValue();

      if (ae.getOperator() == AssignExpr.Operator.ASSIGN && val.isObjectCreationExpr()) {
        emitNewAssign(target, val.asObjectCreationExpr(), out, indent);
        return;
      }

      if (ae.getOperator() == AssignExpr.Operator.ASSIGN) {
        emit(out, indent, target + " = " + expr(val) + ".");
        return;
      }

      // compound best-effort
      String rhs = expr(val);
      switch (ae.getOperator()) {
        case PLUS -> emit(out, indent, target + " = " + target + " + " + rhs + ".");
        case MINUS -> emit(out, indent, target + " = " + target + " - " + rhs + ".");
        case MULTIPLY -> emit(out, indent, target + " = " + target + " * " + rhs + ".");
        case DIVIDE -> emit(out, indent, target + " = " + target + " / " + rhs + ".");
        default -> emit(out, indent, "\" TODO: assignment op not mapped: " + ae.getOperator());
      }
      return;
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

      // no-arg calls: this.foo() -> me->foo( )
      if (mc.getArguments().isEmpty()) {
        emit(out, indent, abapCallNoArgs(mc));
        return;
      }

      emit(out, indent, "\" TODO: method call with args not translated (ABAP needs parameter names): " + mc);
      return;
    }

    if (e.isObjectCreationExpr()) {
      emit(out, indent, "\" TODO: standalone new-expression skipped (needs assignment): " + e);
      return;
    }

    emit(out, indent, "\" TODO: expression not mapped: " + prettyNodeName(e.getClass().getSimpleName()));
  }

  private String abapCallNoArgs(MethodCallExpr mc) {
    String name = mc.getNameAsString();

    if (mc.getScope().isEmpty()) {
      return "me->" + name + "( ).";
    }

    Expression scope = mc.getScope().get();
    if (scope.isThisExpr()) return "me->" + name + "( ).";
    if (scope.isSuperExpr()) return "super->" + name + "( ).";

    if (scope.isNameExpr()) {
      String s = scope.asNameExpr().getNameAsString();
      if (looksLikeTypeName(s)) return "zcl_" + toSnakeLower(s) + "=>" + name + "( ).";
      return s + "->" + name + "( ).";
    }

    if (scope.isFieldAccessExpr()) {
      return accessExpr(scope.asFieldAccessExpr()) + "->" + name + "( ).";
    }

    return expr(scope) + "->" + name + "( ).";
  }

  // ===========================
  // new -> ABAP NEW (argless only safely)
  // ===========================
  private void emitNewVar(String varName, String declaredType, ObjectCreationExpr oce, StringBuilder out, int indent) {
    String created = stripGeneric(oce.getTypeAsString());
    String abapClass = mapJavaTypeToAbapClass(created);

    emit(out, indent, "DATA " + varName + " TYPE REF TO " + abapClass + ".");

    if (!oce.getArguments().isEmpty()) {
      emit(out, indent, "\" TODO: NEW " + abapClass + "( ... ) skipped (constructor parameter names unknown)");
      return;
    }

    emit(out, indent, varName + " = NEW " + abapClass + "( ).");
  }

  private void emitNewAssign(String target, ObjectCreationExpr oce, StringBuilder out, int indent) {
    String created = stripGeneric(oce.getTypeAsString());
    String abapClass = mapJavaTypeToAbapClass(created);

    if (!oce.getArguments().isEmpty()) {
      emit(out, indent, "\" TODO: NEW " + abapClass + "( ... ) skipped (constructor parameter names unknown)");
      return;
    }

    emit(out, indent, target + " = NEW " + abapClass + "( ).");
  }

  // ===========================
  // Expressions
  // ===========================
  private String expr(Expression e) {
    if (e == null) return "''";

    if (e.isThisExpr()) return "me";
    if (e.isSuperExpr()) return "super";
    if (e.isNullLiteralExpr()) return "INITIAL";

    if (e.isNameExpr()) {
      String n = e.asNameExpr().getNameAsString();
      return resolveEnumConstantUnqualified(n).orElse(n);
    }

    if (e.isFieldAccessExpr()) {
      FieldAccessExpr fa = e.asFieldAccessExpr();
      if (fa.getScope().isThisExpr()) return "me->" + fa.getNameAsString();

      if (fa.getScope().isNameExpr()) {
        String left = fa.getScope().asNameExpr().getNameAsString();
        Optional<String> enumConst = resolveEnumConstantQualified(left, fa.getNameAsString());
        if (enumConst.isPresent()) return enumConst.get();
        if (looksLikeTypeName(left)) return "zcl_" + toSnakeLower(left) + "=>" + fa.getNameAsString();
        return left + "->" + fa.getNameAsString();
      }

      if (fa.getScope().isFieldAccessExpr()) {
        return accessExpr(fa.getScope().asFieldAccessExpr()) + "->" + fa.getNameAsString();
      }

      return expr(fa.getScope()) + "->" + fa.getNameAsString();
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

    if (e.isObjectCreationExpr()) {
      ObjectCreationExpr oce = e.asObjectCreationExpr();
      if (!oce.getArguments().isEmpty()) return "''";
      return "NEW " + mapJavaTypeToAbapClass(stripGeneric(oce.getTypeAsString())) + "( )";
    }

    return "''";
  }

  private String accessExpr(FieldAccessExpr fa) {
    Expression scope = fa.getScope();
    String name = fa.getNameAsString();

    if (scope.isThisExpr()) return "me->" + name;

    if (scope.isNameExpr()) {
      String left = scope.asNameExpr().getNameAsString();
      if (looksLikeTypeName(left)) return "zcl_" + toSnakeLower(left) + "=>" + name;
      return left + "->" + name;
    }

    if (scope.isFieldAccessExpr()) {
      return accessExpr(scope.asFieldAccessExpr()) + "->" + name;
    }

    return expr(scope) + "->" + name;
  }

  // ===========================
  // Type mapping
  // ===========================
  private String mapTypeForAbap(String javaType) {
    String tRaw = javaType == null ? "" : javaType.trim();
    String t = stripGeneric(tRaw);

    // arrays: T[] -> STANDARD TABLE OF T
    if (tRaw.endsWith("[]")) {
      String elem = tRaw.substring(0, tRaw.length() - 2).trim();
      return "STANDARD TABLE OF " + mapTypeForAbap(elem) + " WITH EMPTY KEY";
    }

    // inferred/declared enums
    if (enumConstantsByType.containsKey(t)) {
      return "ty_" + toSnakeLower(t);
    }

    return switch (t) {
      case "int", "Integer", "long", "Long", "short", "Short", "byte", "Byte" -> "i";
      case "double", "Double", "float", "Float" -> "f";
      case "boolean", "Boolean" -> "abap_bool";
      case "String" -> "string";
      default -> {
        if (looksLikeTypeName(t)) yield "REF TO " + mapJavaTypeToAbapClass(t);
        yield "string";
      }
    };
  }

  private String mapJavaTypeToAbapClass(String javaType) {
    String t = stripGeneric(javaType);
    if (t.isBlank()) return "object";
    return "zcl_" + toSnakeLower(t);
  }

  // ===========================
  // Parse robustness (brace fix)
  // ===========================
  private record ParseAttempt(boolean ok, CompilationUnit cu, String fixedSource, String errorMessage) {}

  private ParseAttempt tryParseCompilationUnit(String src) {
    String fixed = balanceBraces(normalize(src));
    try {
      CompilationUnit cu = StaticJavaParser.parse(fixed);
      return new ParseAttempt(true, cu, fixed, null);
    } catch (Exception e1) {
      // retry without balancing (sometimes balancing makes it worse)
      try {
        CompilationUnit cu2 = StaticJavaParser.parse(normalize(src));
        return new ParseAttempt(true, cu2, normalize(src), null);
      } catch (Exception e2) {
        return new ParseAttempt(false, null, fixed, shortMessage(e2));
      }
    }
  }

  private String balanceBraces(String src) {
    if (src == null) return "";
    String s = src;

    int depth = 0;
    boolean inStr = false;
    char quote = 0;
    boolean inLineComment = false;
    boolean inBlockComment = false;

    for (int i = 0; i < s.length(); i++) {
      char c = s.charAt(i);
      char n = (i + 1 < s.length()) ? s.charAt(i + 1) : 0;

      if (inLineComment) {
        if (c == '\n') inLineComment = false;
        continue;
      }
      if (inBlockComment) {
        if (c == '*' && n == '/') { inBlockComment = false; i++; }
        continue;
      }

      if (!inStr) {
        if (c == '/' && n == '/') { inLineComment = true; i++; continue; }
        if (c == '/' && n == '*') { inBlockComment = true; i++; continue; }
      }

      if (inStr) {
        if (c == '\\') { i++; continue; }
        if (c == quote) { inStr = false; quote = 0; }
        continue;
      } else if (c == '"' || c == '\'') {
        inStr = true; quote = c;
        continue;
      }

      if (c == '{') depth++;
      if (c == '}') depth = Math.max(0, depth - 1);
    }

    if (depth <= 0) return s;

    StringBuilder out = new StringBuilder(s);
    out.append("\n");
    for (int i = 0; i < depth; i++) out.append("}\n");
    return out.toString();
  }

  // ===========================
  // Mixed extraction for snippet
  // ===========================
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

  // ===========================
  // System.out
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
  // Comments
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

  private String ensureTrailingNewline(StringBuilder out) {
    String s = out.toString().replace("\r\n", "\n").replace("\r", "\n").trim();
    return s.isEmpty() ? "\n" : (s + "\n");
  }

  private void resetState() {
    enumConstantsByType.clear();
    enumTypesByConstant.clear();
    declaredTemps.clear();
  }

  private String normalize(String s) {
    if (s == null) return "";
    return s.replace("\r\n", "\n").replace("\r", "\n").trim();
  }

  private String stripGeneric(String t) {
    if (t == null) return "";
    int lt = t.indexOf('<');
    return lt >= 0 ? t.substring(0, lt).trim() : t.trim();
  }

  private boolean looksLikeTypeName(String n) {
    return n != null && !n.isBlank() && Character.isUpperCase(n.charAt(0));
  }

  private String toSnakeLower(String s) {
    if (s == null || s.isBlank()) return "demo";
    String x = s.replaceAll("([a-z])([A-Z])", "$1_$2");
    return x.toLowerCase(Locale.ROOT);
  }

  private String prettyNodeName(String simple) {
    if (simple == null) return "Node";
    String s = simple.replace("Stmt", " statement").replace("Expr", " expression");
    s = s.replaceAll("([a-z])([A-Z])", "$1 $2");
    return s.trim();
  }

  private String shortMessage(Exception e) {
    String msg = e.getMessage();
    if (msg == null) return e.getClass().getSimpleName();
    String[] lines = msg.split("\n");
    return lines.length > 0 ? lines[0] : msg;
  }

  private boolean isSimpleLiteral(Expression e) {
    if (e == null) return false;
    return e.isIntegerLiteralExpr() || e.isBooleanLiteralExpr() || e.isStringLiteralExpr()
        || e.isCharLiteralExpr() || e.isDoubleLiteralExpr() || e.isLongLiteralExpr();
  }
}
