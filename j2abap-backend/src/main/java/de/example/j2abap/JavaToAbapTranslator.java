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

  // ===========================
  // per-translation state
  // ===========================
  private final Map<String, List<String>> enumConstantsByType = new LinkedHashMap<>();
  private final Map<String, List<String>> enumTypesByConstant = new HashMap<>();

  // ===========================
  // Public API
  // ===========================
  public String translateAuto(String javaCode) {
    String src = normalize(javaCode);
    if (src.isBlank()) return "\n";

    // Prefer class if parseable and has at least one top-level type
    ParseAttempt a = tryParseCompilationUnit(src);
    if (a.ok && a.cu != null && !a.cu.getTypes().isEmpty()) {
      return translateClassInternal(a.fixedSource);
    }

    // Otherwise snippet
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
  // ===========================
  private String translateSnippetInternal(String src) {
    resetState();
    StringBuilder out = new StringBuilder();

    Extracted ex = extractTopLevelTypeDeclsAnywhere(src);

    // If user selected snippet but only provided class/enum/etc, avoid empty output
    if ((ex.remainingCode == null || ex.remainingCode.isBlank())
        && ex.extractedTypes != null && !ex.extractedTypes.isBlank()) {
      emit(out, 0, "\" Hinweis: „Snippet“ gewählt, aber dein Input enthält nur class/enum/record/interface.");
      emit(out, 0, "\" Bitte „Class“ wählen oder nur Statements einfügen.");
      return ensureTrailingNewline(out);
    }

    // Wrap types + statements into a dummy class/method so JavaParser can parse everything together.
    String wrapper =
        "class __J2ABAP {\n" +
            ex.extractedTypes +
            "  void __m() {\n" +
            ex.remainingCode + "\n" +
            "  }\n" +
            "}\n";

    try {
      CompilationUnit cu = StaticJavaParser.parse(balanceBraces(wrapper));

      indexEnums(cu);
      inferEnumsFromSwitches(cu);

      // Emit inferred/declared enums at top (snippet only)
      emitEnumDeclarationsSnippet(out);

      MethodDeclaration m = cu.findFirst(MethodDeclaration.class, md -> md.getNameAsString().equals("__m"))
          .orElse(null);

      if (m == null || m.getBody().isEmpty()) {
        emit(out, 0, "\" TODO: keine Statements gefunden");
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
  // ===========================
  private String translateClassInternal(String src) {
    resetState();
    StringBuilder out = new StringBuilder();

    ParseAttempt a = tryParseCompilationUnit(src);
    if (!a.ok || a.cu == null) {
      out.append("Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/interface/record/enum.\n");
      if (a.errorMessage != null) out.append(a.errorMessage).append("\n");
      return ensureTrailingNewline(out);
    }

    CompilationUnit cu = a.cu;

    emitCompilationUnitHeaderComments(cu, out);

    indexEnums(cu);
    inferEnumsFromSwitches(cu);

    Optional<ClassOrInterfaceDeclaration> clsOpt = cu.findFirst(ClassOrInterfaceDeclaration.class);
    Optional<RecordDeclaration> recOpt = cu.findFirst(RecordDeclaration.class);
    Optional<EnumDeclaration> enumOpt = cu.findFirst(EnumDeclaration.class);

    // If the file is only an enum, output a helpful ABAP snippet (no class wrapper)
    if (clsOpt.isEmpty() && recOpt.isEmpty() && enumOpt.isPresent()) {
      emitEnumDeclarationsSnippet(out);
      return ensureTrailingNewline(out);
    }

    if (recOpt.isPresent() && clsOpt.isEmpty()) {
      return translateRecordAsClass(recOpt.get(), out);
    }

    if (clsOpt.isEmpty()) {
      out.append("Parse error (Java). Tipp: Keine class/interface/record/enum gefunden.\n");
      return ensureTrailingNewline(out);
    }

    ClassOrInterfaceDeclaration cls = clsOpt.get();
    if (cls.isInterface()) return translateInterface(cls, out);

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

    emitEnumDeclarationsInClassPublic(out);

    // Fields
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

        // Optional initializer
        if (v.getInitializer().isPresent()) {
          Expression init = v.getInitializer().get();
          if (init.isObjectCreationExpr()) {
            // For class mode we prefer explicit REF+NEW if possible
            emitTodoJava(out, 4, v.toString() + ";", "Initialisierer 'new ...' bitte in constructor/Methoden übersetzen");
          } else {
            // best-effort direct assignment only if expression is safe
            ExprRes er = tryExpr(init);
            if (er.ok) emit(out, 4, name + " = " + er.abap + ".");
            else emitTodoJava(out, 4, v.toString() + ";", er.reason);
          }
        }
      }
    }

    // Constructor (only first)
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

    // Methods signatures
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
        emit(out, 2, "\" TODO: mehrere Java-Konstruktoren; ABAP hat kein Overload (nur der erste wurde übersetzt).");
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
  // Statements (core)
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
      ExprRes cond = tryExpr(is.getCondition());
      if (!cond.ok) {
        emitTodoJava(out, indent, st.toString(), cond.reason);
        return;
      }
      emit(out, indent, "IF " + cond.abap + ".");
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
      ExprRes cond = tryExpr(ws.getCondition());
      if (!cond.ok) {
        emitTodoJava(out, indent, st.toString(), cond.reason);
        return;
      }
      emit(out, indent, "WHILE " + cond.abap + ".");
      translateStatement(ws.getBody(), out, indent + 2);
      emit(out, indent, "ENDWHILE.");
      return;
    }

    if (st.isForEachStmt()) {
      ForEachStmt fes = st.asForEachStmt();
      String var = fes.getVariable().getVariable(0).getNameAsString();
      ExprRes it = tryExpr(fes.getIterable());
      if (!it.ok) {
        emitTodoJava(out, indent, st.toString(), it.reason);
        return;
      }
      emit(out, indent, "LOOP AT " + it.abap + " INTO " + var + ".");
      translateStatement(fes.getBody(), out, indent + 2);
      emit(out, indent, "ENDLOOP.");
      return;
    }

    if (st.isReturnStmt()) {
      ReturnStmt rs = st.asReturnStmt();
      if (rs.getExpression().isPresent()) {
        ExprRes er = tryExpr(rs.getExpression().get());
        if (!er.ok) {
          emitTodoJava(out, indent, st.toString(), er.reason);
          return;
        }
        emit(out, indent, "rv_result = " + er.abap + ".");
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

    // default: preserve original Java as TODO
    emitTodoJava(out, indent, st.toString(), "Statement nicht unterstützt / nicht eindeutig in ABAP");
  }

  private void translateTryStmt(TryStmt ts, StringBuilder out, int indent) {
    emit(out, indent, "TRY.");
    for (Statement st : ts.getTryBlock().getStatements()) translateStatement(st, out, indent + 2);

    for (CatchClause cc : ts.getCatchClauses()) {
      String var = cc.getParameter().getNameAsString();
      emit(out, indent, "CATCH cx_root INTO DATA(" + var + ").");
      emit(out, indent + 2, "\" TODO: Java exception type nicht gemappt: " + cc.getParameter().getTypeAsString());
      for (Statement st : cc.getBody().getStatements()) translateStatement(st, out, indent + 2);
    }

    if (ts.getFinallyBlock().isPresent()) {
      emit(out, indent, "FINALLY.");
      for (Statement st : ts.getFinallyBlock().get().getStatements()) translateStatement(st, out, indent + 2);
    }

    emit(out, indent, "ENDTRY.");
  }

  private void translateSwitchStmt(SwitchStmt ss, StringBuilder out, int indent) {
    ExprRes selector = tryExpr(ss.getSelector());
    if (!selector.ok) {
      emitTodoJava(out, indent, ss.toString(), selector.reason);
      return;
    }

    emit(out, indent, "CASE " + selector.abap + ".");

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

    ExprRes er = tryExpr(e);
    return er.ok ? er.abap : "''";
  }

  // ===========================
  // Expression statements
  // ===========================
  private void translateExpressionStmt(ExpressionStmt es, StringBuilder out, int indent) {
    Expression e = es.getExpression();
    emitLeadingComments(e, out, indent);

    // Variable declarations
    if (e.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = e.asVariableDeclarationExpr();
      for (VariableDeclarator v : vde.getVariables()) {
        translateVarDeclarator(v, out, indent);
      }
      return;
    }

    // Assignments
    if (e.isAssignExpr()) {
      AssignExpr ae = e.asAssignExpr();
      translateAssignExpr(ae, out, indent);
      return;
    }

    // Method call as statement
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

      // no-arg calls -> translate
      if (mc.getArguments().isEmpty()) {
        emit(out, indent, abapCallNoArgs(mc));
        return;
      }

      // args present -> keep as TODO with original Java
      emitTodoJava(out, indent, es.toString(), "Method call mit Argumenten nicht eindeutig (ABAP braucht Parameternamen)");
      return;
    }

    // new as standalone statement -> TODO
    if (e.isObjectCreationExpr()) {
      emitTodoJava(out, indent, es.toString(), "'new' als Statement ohne Zuweisung");
      return;
    }

    // fallback
    emitTodoJava(out, indent, es.toString(), "Expression nicht unterstützt / nicht eindeutig");
  }

  private void translateVarDeclarator(VariableDeclarator v, StringBuilder out, int indent) {
    String name = v.getNameAsString();
    String javaType = v.getTypeAsString();

    // new X()
    if (v.getInitializer().isPresent() && v.getInitializer().get().isObjectCreationExpr()) {
      ObjectCreationExpr oce = v.getInitializer().get().asObjectCreationExpr();
      emitNewVar(name, javaType, oce, out, indent);
      return;
    }

    // normal declaration
    emit(out, indent, "DATA " + name + " TYPE " + mapTypeForAbap(javaType) + ".");

    if (v.getInitializer().isPresent()) {
      Expression init = v.getInitializer().get();

      // handle method call initializer if we can
      if (init.isMethodCallExpr()) {
        ExprRes er = tryExpr(init);
        if (er.ok) {
          emit(out, indent, name + " = " + er.abap + ".");
        } else {
          emitTodoJava(out, indent, v.toString() + ";", er.reason);
        }
        return;
      }

      ExprRes er = tryExpr(init);
      if (er.ok) emit(out, indent, name + " = " + er.abap + ".");
      else emitTodoJava(out, indent, v.toString() + ";", er.reason);
    }
  }

  private void translateAssignExpr(AssignExpr ae, StringBuilder out, int indent) {
    Expression targetExpr = ae.getTarget();
    Expression valueExpr = ae.getValue();

    // Special: i = i++ / i = ++i / i = i-- / i = --i  (Java semantics: value may or may not change)
    if (ae.getOperator() == AssignExpr.Operator.ASSIGN && valueExpr.isUnaryExpr()) {
      UnaryExpr u = valueExpr.asUnaryExpr();
      UnaryExpr.Operator op = u.getOperator();

      boolean sameVar =
          u.getExpression().isNameExpr()
              && targetExpr.isNameExpr()
              && u.getExpression().asNameExpr().getNameAsString()
              .equals(targetExpr.asNameExpr().getNameAsString());

      if (sameVar &&
          (op == UnaryExpr.Operator.POSTFIX_INCREMENT ||
              op == UnaryExpr.Operator.PREFIX_INCREMENT ||
              op == UnaryExpr.Operator.POSTFIX_DECREMENT ||
              op == UnaryExpr.Operator.PREFIX_DECREMENT)) {

        String t = expr(targetExpr);

        // Java: i = i++; => i bleibt gleich (Nebenwirkung wird durch Zuweisung neutralisiert)
        emit(out, indent, "\" TODO: Java \"" + ae.toString() + "\" ist trickreich; Effekt: Wert bleibt gleich.");
        emit(out, indent, t + " = " + t + ".");
        return;
      }
    }

    // new assignment
    if (ae.getOperator() == AssignExpr.Operator.ASSIGN && valueExpr.isObjectCreationExpr()) {
      emitNewAssign(expr(targetExpr), valueExpr.asObjectCreationExpr(), out, indent);
      return;
    }

    String target = expr(targetExpr);

    // compound assignment best-effort if RHS expression is safe
    if (ae.getOperator() != AssignExpr.Operator.ASSIGN) {
      ExprRes rhs = tryExpr(valueExpr);
      if (!rhs.ok) {
        emitTodoJava(out, indent, ae.toString() + ";", rhs.reason);
        return;
      }

      switch (ae.getOperator()) {
        case PLUS -> emit(out, indent, target + " = " + target + " + " + rhs.abap + ".");
        case MINUS -> emit(out, indent, target + " = " + target + " - " + rhs.abap + ".");
        case MULTIPLY -> emit(out, indent, target + " = " + target + " * " + rhs.abap + ".");
        case DIVIDE -> emit(out, indent, target + " = " + target + " / " + rhs.abap + ".");
        default -> emitTodoJava(out, indent, ae.toString() + ";", "compound assignment nicht gemappt: " + ae.getOperator());
      }
      return;
    }

    // plain assign
    ExprRes rhs = tryExpr(valueExpr);
    if (!rhs.ok) {
      emitTodoJava(out, indent, ae.toString() + ";", rhs.reason);
      return;
    }
    emit(out, indent, target + " = " + rhs.abap + ".");
  }

  // ===========================
  // Expression translation (safe)
  // ===========================
  private record ExprRes(boolean ok, String abap, String reason) {}

  private ExprRes tryExpr(Expression e) {
    try {
      return new ExprRes(true, expr(e), null);
    } catch (UnsupportedExpression ex) {
      return new ExprRes(false, "''", ex.getMessage());
    } catch (Exception ex) {
      return new ExprRes(false, "''", "Expression nicht übersetzbar");
    }
  }

  private static final class UnsupportedExpression extends RuntimeException {
    UnsupportedExpression(String msg) { super(msg); }
  }

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

      // this.x -> me->x
      if (fa.getScope().isThisExpr()) return "me->" + fa.getNameAsString();

      // Day.MONDAY -> c_day_monday
      if (fa.getScope().isNameExpr()) {
        String left = fa.getScope().asNameExpr().getNameAsString();
        Optional<String> enumConst = resolveEnumConstantQualified(left, fa.getNameAsString());
        if (enumConst.isPresent()) return enumConst.get();

        // ClassName.CONST -> zcl_class_name=>const
        if (looksLikeTypeName(left)) return "zcl_" + toSnakeLower(left) + "=>" + fa.getNameAsString();

        // obj.field -> obj->field
        return left + "->" + fa.getNameAsString();
      }

      // chained: a.b.c
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
      // ++/-- as expression is tricky; statement-level handles common cases
      throw new UnsupportedExpression("Unary ++/-- als Expression nicht sicher in ABAP übersetzbar");
    }

    // list.get(0) best-effort -> list[ 1 ]
    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();

      if (isSystemOutPrintln(mc) || isSystemOutPrint(mc)) {
        throw new UnsupportedExpression("System.out.* als Expression nicht sinnvoll");
      }

      // x.get(0)
      if (mc.getNameAsString().equals("get") && mc.getArguments().size() == 1 && mc.getScope().isPresent()) {
        Expression idx = mc.getArgument(0);
        if (idx.isIntegerLiteralExpr()) {
          int i0 = Integer.parseInt(idx.asIntegerLiteralExpr().getValue());
          int i1 = i0 + 1; // Java 0-based -> ABAP 1-based for table expr
          String table = abapValue(mc.getScope().get());
          return table + "[ " + i1 + " ]";
        }
        // if idx is variable/expression: still best-effort
        String table = abapValue(mc.getScope().get());
        return table + "[ " + expr(idx) + " + 1 ]";
      }

      throw new UnsupportedExpression("Method call als Expression nicht eindeutig (z.B. ABAP Parameternamen/Tabellentyp fehlen)");
    }

    // new X() as expression -> only safe without args
    if (e.isObjectCreationExpr()) {
      ObjectCreationExpr oce = e.asObjectCreationExpr();
      if (!oce.getArguments().isEmpty()) {
        throw new UnsupportedExpression("new ... mit Argumenten: ABAP braucht Parameternamen");
      }
      return "NEW " + mapJavaTypeToAbapClass(stripGeneric(oce.getTypeAsString())) + "( )";
    }

    if (e.isConditionalExpr()) {
      return "COND #( WHEN " + expr(e.asConditionalExpr().getCondition()) +
          " THEN " + expr(e.asConditionalExpr().getThenExpr()) +
          " ELSE " + expr(e.asConditionalExpr().getElseExpr()) + " )";
    }

    throw new UnsupportedExpression("Expression-Typ nicht unterstützt: " + e.getClass().getSimpleName());
  }

  private String abapValue(Expression scope) {
    if (scope == null) return "me";
    if (scope.isThisExpr()) return "me";
    if (scope.isNameExpr()) return scope.asNameExpr().getNameAsString();
    if (scope.isFieldAccessExpr()) return accessExpr(scope.asFieldAccessExpr());
    return expr(scope);
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

  private String abapCallNoArgs(MethodCallExpr mc) {
    String name = mc.getNameAsString();

    if (mc.getScope().isEmpty()) return "me->" + name + "( ).";

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
  // new -> ABAP NEW
  // ===========================
  private void emitNewVar(String varName, String declaredType, ObjectCreationExpr oce, StringBuilder out, int indent) {
    String created = stripGeneric(oce.getTypeAsString());
    String abapClass = mapJavaTypeToAbapClass(created);

    emit(out, indent, "DATA " + varName + " TYPE REF TO " + abapClass + ".");

    if (!oce.getArguments().isEmpty()) {
      emitTodoJava(out, indent, varName + " = " + oce.toString() + ";", "Konstruktor-Argumente: ABAP braucht Parameternamen");
      return;
    }

    emit(out, indent, varName + " = NEW " + abapClass + "( ).");
  }

  private void emitNewAssign(String target, ObjectCreationExpr oce, StringBuilder out, int indent) {
    String created = stripGeneric(oce.getTypeAsString());
    String abapClass = mapJavaTypeToAbapClass(created);

    if (!oce.getArguments().isEmpty()) {
      emitTodoJava(out, indent, target + " = " + oce.toString() + ";", "Konstruktor-Argumente: ABAP braucht Parameternamen");
      return;
    }

    emit(out, indent, target + " = NEW " + abapClass + "( ).");
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
   * infer Day = [MONDAY, FRIDAY, ...] so case labels can be mapped.
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
    Optional<MethodDeclaration> md = node.findAncestor(MethodDeclaration.class);
    if (md.isPresent()) {
      for (Parameter p : md.get().getParameters()) {
        if (p.getNameAsString().equals(varName)) return p.getTypeAsString();
      }
      for (VariableDeclarator vd : md.get().findAll(VariableDeclarator.class)) {
        if (vd.getNameAsString().equals(varName)) return vd.getTypeAsString();
      }
    }

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
  // System.out helpers
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
  // TODO output (requested)
  // ===========================
  private void emitTodoJava(StringBuilder out, int indent, String javaCode, String reason) {
    String j = (javaCode == null ? "" : javaCode).replace("\r\n", "\n").replace("\r", "\n").trim();
    if (j.isEmpty()) {
      emit(out, indent, "\" TODO: (leer) (weil: " + safe(reason) + ")");
      return;
    }

    String[] lines = j.split("\n");
    // first line with reason
    emit(out, indent, "\" TODO: " + lines[0].trim() + (reason == null ? "" : "  (weil: " + safe(reason) + ")"));
    // following lines
    for (int i = 1; i < lines.length; i++) {
      String t = lines[i].trim();
      if (!t.isEmpty()) emit(out, indent, "\"       " + t);
    }
  }

  private String safe(String s) {
    if (s == null) return "";
    return s.replace("\"", "'").trim();
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

    // enums -> ty_x
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
  // Parse robustness
  // ===========================
  private record ParseAttempt(boolean ok, CompilationUnit cu, String fixedSource, String errorMessage) {}

  private ParseAttempt tryParseCompilationUnit(String src) {
    String fixed = balanceBraces(normalize(src));
    try {
      CompilationUnit cu = StaticJavaParser.parse(fixed);
      return new ParseAttempt(true, cu, fixed, null);
    } catch (Exception e1) {
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
