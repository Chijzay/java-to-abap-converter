package de.example.j2abap;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.*;
import com.github.javaparser.ast.comments.BlockComment;
import com.github.javaparser.ast.comments.Comment;
import com.github.javaparser.ast.comments.JavadocComment;
import com.github.javaparser.ast.comments.LineComment;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.stmt.*;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * Java -> ABAP best-effort translator (JavaParser-based).
 *
 * Highlights:
 * - "classic for" (for i=0; i<names.length; i++) -> LOOP AT names INTO DATA(name).
 * - String[] init {"a","b"} -> ABAP VALUE #( ( 'a' ) ( 'b' ) )
 * - names[i] -> name (when loop-substitution is safe) OR names[ i + 1 ] fallback
 * - .equals(...) / .equalsIgnoreCase(...)
 * - Java string concat with '+' -> ABAP string template |...{ }...|
 * - break -> EXIT.
 */
@Service
public class JavaToAbapTranslator {

  // ===========================
  // per-translation state
  // ===========================
  private final Map<String, List<String>> enumConstantsByType = new LinkedHashMap<>();
  private final Map<String, List<String>> enumTypesByConstant = new HashMap<>();

  // map: "<arrayName>#<indexVar>" -> "<loopVar>" (for-loop optimization substitution)
  private final Map<String, String> arrayIndexSubst = new HashMap<>();

  private enum OutputStyle { LIST_WRITE, CLASSRUN_OUT }

  // ===========================
  // Public API (synchronized because this Spring @Service keeps state)
  // ===========================
  public synchronized String translateAuto(String javaCode) {
    String src = normalize(javaCode);
    if (src.isBlank()) return "\n";

    ParseAttempt a = tryParseCompilationUnit(src);
    if (a.ok && a.cu != null && !a.cu.getTypes().isEmpty()) {
      return translateClassInternal(a.fixedSource);
    }

    return translateSnippetInternal(src);
  }

  public synchronized String translateSnippet(String javaCode) {
    return translateSnippetInternal(normalize(javaCode));
  }

  public synchronized String translateClass(String javaCode) {
    return translateClassInternal(normalize(javaCode));
  }

  public synchronized String translate(String javaCode, String mode) {
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

    if ((ex.remainingCode == null || ex.remainingCode.isBlank())
        && ex.extractedTypes != null && !ex.extractedTypes.isBlank()) {
      emit(out, 0, "\" Hinweis: „Snippet“ gewählt, aber dein Input enthält nur class/enum/record/interface.");
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
      CompilationUnit cu = StaticJavaParser.parse(balanceBraces(wrapper));

      indexEnums(cu);
      inferEnumsFromSwitches(cu);

      emitEnumDeclarationsSnippet(out);

      MethodDeclaration m = cu.findFirst(MethodDeclaration.class, md -> md.getNameAsString().equals("__m"))
          .orElse(null);

      if (m == null || m.getBody().isEmpty()) {
        emit(out, 0, "\" TODO: keine Statements gefunden");
        return ensureTrailingNewline(out);
      }

      for (Statement st : m.getBody().get().getStatements()) {
        translateStatement(st, out, 0, OutputStyle.LIST_WRITE);
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

    MethodDeclaration javaMain = findJavaMain(cls).orElse(null);
    boolean hasMain = (javaMain != null);

    out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");
    out.append("  PUBLIC SECTION.\n");

    if (hasMain) {
      out.append("    INTERFACES if_oo_adt_classrun.\n");
    }

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
      }
    }

    // Constructor signature (only first)
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

    // Methods signatures (skip Java main; we map it to IF_OO_ADT_CLASSRUN~MAIN)
    for (MethodDeclaration md : cls.getMethods()) {
      if (hasMain && md == javaMain) continue;

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

    // Java main -> if_oo_adt_classrun~main
    if (hasMain) {
      out.append("  METHOD if_oo_adt_classrun~main.\n");
      if (javaMain.getBody().isPresent()) {
        for (Statement st : javaMain.getBody().get().getStatements()) {
          translateStatement(st, out, 4, OutputStyle.CLASSRUN_OUT);
        }
      } else {
        emit(out, 4, "\" TODO: main body missing");
      }
      out.append("  ENDMETHOD.\n\n");
    }

    // Constructor body
    if (!ctors.isEmpty()) {
      ConstructorDeclaration cd = ctors.get(0);
      out.append("  METHOD constructor.\n");
      for (Statement st : cd.getBody().getStatements()) {
        translateStatement(st, out, 4, OutputStyle.CLASSRUN_OUT);
      }
      out.append("  ENDMETHOD.\n\n");

      if (ctors.size() > 1) {
        emit(out, 2, "\" TODO: mehrere Java-Konstruktoren; ABAP hat kein Overload (nur der erste wurde übersetzt).");
        out.append("\n");
      }
    }

    // Methods bodies
    for (MethodDeclaration md : cls.getMethods()) {
      if (hasMain && md == javaMain) continue;

      out.append("  METHOD ").append(md.getNameAsString()).append(".\n");

      if (md.getBody().isPresent()) {
        for (Statement st : md.getBody().get().getStatements()) {
          translateStatement(st, out, 4, OutputStyle.CLASSRUN_OUT);
        }
      } else {
        emit(out, 4, "\" TODO: method body missing");
      }

      out.append("  ENDMETHOD.\n\n");
    }

    out.append("ENDCLASS.\n");
    return ensureTrailingNewline(out);
  }

  private Optional<MethodDeclaration> findJavaMain(ClassOrInterfaceDeclaration cls) {
    for (MethodDeclaration md : cls.getMethods()) {
      if (!md.getNameAsString().equals("main")) continue;
      if (!md.isStatic()) continue;
      if (!md.getType().isVoidType()) continue;
      if (md.getParameters().size() != 1) continue;

      String t = md.getParameter(0).getTypeAsString().replace(" ", "");
      if (t.equals("String[]") || t.equals("String...")) return Optional.of(md);
    }
    return Optional.empty();
  }

  // ===========================
  // Statements (core)
  // ===========================
  private void translateStatement(Statement st, StringBuilder out, int indent, OutputStyle style) {
    if (st == null) return;

    emitLeadingComments(st, out, indent);

    if (st.isBlockStmt()) {
      for (Statement inner : st.asBlockStmt().getStatements()) translateStatement(inner, out, indent, style);
      return;
    }
    if (st.isEmptyStmt()) return;

    if (st.isExpressionStmt()) {
      translateExpressionStmt(st.asExpressionStmt(), out, indent, style);
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
      translateStatement(is.getThenStmt(), out, indent + 2, style);
      if (is.getElseStmt().isPresent()) {
        emit(out, indent, "ELSE.");
        translateStatement(is.getElseStmt().get(), out, indent + 2, style);
      }
      emit(out, indent, "ENDIF.");
      return;
    }

    if (st.isSwitchStmt()) {
      translateSwitchStmt(st.asSwitchStmt(), out, indent, style);
      return;
    }

    if (st.isTryStmt()) {
      translateTryStmt(st.asTryStmt(), out, indent, style);
      return;
    }

    // --------- NEW: classic for-loop support ----------
    if (st.isForStmt()) {
      translateForStmt(st.asForStmt(), out, indent, style);
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
      translateStatement(ws.getBody(), out, indent + 2, style);
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
      translateStatement(fes.getBody(), out, indent + 2, style);
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

    emitTodoJava(out, indent, st.toString(), "Statement nicht unterstützt / nicht eindeutig in ABAP");
  }

  // ===========================
  // ForStmt (classic for-loop) -> LOOP AT (if safe) else WHILE fallback
  // ===========================
  private void translateForStmt(ForStmt fs, StringBuilder out, int indent, OutputStyle style) {
    emitLeadingComments(fs, out, indent);

    ClassicForHeader h = parseClassicForHeader(fs);
    if (h == null || fs.getCompare().isEmpty()) {
      emitTodoJava(out, indent, fs.toString(), "classic for nicht unterstützt");
      return;
    }

    if (tryTranslateForAsLoopAt(fs, h, out, indent, style)) return;

    // fallback: init; WHILE compare. body. update. ENDWHILE.
    for (Expression init : fs.getInitialization()) {
      emitForInitExpr(init, out, indent, style);
    }

    ExprRes cond = tryExpr(fs.getCompare().get());
    if (!cond.ok) {
      emitTodoJava(out, indent, fs.toString(), cond.reason);
      return;
    }

    emit(out, indent, "WHILE " + cond.abap + ".");
    translateStatement(fs.getBody(), out, indent + 2, style);

    for (Expression upd : fs.getUpdate()) {
      emitForUpdateExpr(upd, out, indent + 2, style);
    }

    emit(out, indent, "ENDWHILE.");
  }

  private boolean tryTranslateForAsLoopAt(ForStmt fs, ClassicForHeader h, StringBuilder out, int indent, OutputStyle style) {
    // pattern: i=0; i<names.length; i++
    if (h.startValue != 0) return false;
    if (h.op != BinaryExpr.Operator.LESS) return false;
    if (h.update == null || !isIncrementExpr(h.update, h.idxVar)) return false;

    if (!(h.right instanceof FieldAccessExpr fa)) return false;
    if (!fa.getNameAsString().equals("length")) return false;
    if (!fa.getScope().isNameExpr()) return false;

    String arrName = fa.getScope().asNameExpr().getNameAsString();

    if (!indexOnlyUsedForArrayAccess(fs.getBody(), h.idxVar, arrName)) return false;

    String loopVar = guessLoopVarName(arrName);

    emit(out, indent, "LOOP AT " + arrName + " INTO DATA(" + loopVar + ").");

    String key = arrName + "#" + h.idxVar;
    arrayIndexSubst.put(key, loopVar);
    try {
      translateStatement(fs.getBody(), out, indent + 2, style);
    } finally {
      arrayIndexSubst.remove(key);
    }

    emit(out, indent, "ENDLOOP.");
    return true;
  }

  private void emitForInitExpr(Expression init, StringBuilder out, int indent, OutputStyle style) {
    if (init == null) return;

    if (init.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = init.asVariableDeclarationExpr();
      for (VariableDeclarator v : vde.getVariables()) translateVarDeclarator(v, out, indent, style);
      return;
    }

    if (init.isAssignExpr()) {
      translateAssignExpr(init.asAssignExpr(), out, indent, style);
      return;
    }

    emitTodoJava(out, indent, init.toString() + ";", "for-init expression nicht unterstützt");
  }

  private void emitForUpdateExpr(Expression upd, StringBuilder out, int indent, OutputStyle style) {
    if (upd == null) return;

    if (upd.isUnaryExpr()) {
      UnaryExpr u = upd.asUnaryExpr();
      if (u.getExpression().isNameExpr()) {
        String v = u.getExpression().asNameExpr().getNameAsString();
        if (u.getOperator() == UnaryExpr.Operator.POSTFIX_INCREMENT || u.getOperator() == UnaryExpr.Operator.PREFIX_INCREMENT) {
          emit(out, indent, v + " = " + v + " + 1.");
          return;
        }
        if (u.getOperator() == UnaryExpr.Operator.POSTFIX_DECREMENT || u.getOperator() == UnaryExpr.Operator.PREFIX_DECREMENT) {
          emit(out, indent, v + " = " + v + " - 1.");
          return;
        }
      }
      emitTodoJava(out, indent, upd.toString() + ";", "unary update nicht unterstützt");
      return;
    }

    if (upd.isAssignExpr()) {
      translateAssignExpr(upd.asAssignExpr(), out, indent, style);
      return;
    }

    emitTodoJava(out, indent, upd.toString() + ";", "for-update expression nicht unterstützt");
  }

  private boolean isIncrementExpr(Expression e, String var) {
    if (e == null) return false;

    if (e.isUnaryExpr()) {
      UnaryExpr u = e.asUnaryExpr();
      if (!u.getExpression().isNameExpr()) return false;
      if (!u.getExpression().asNameExpr().getNameAsString().equals(var)) return false;
      return u.getOperator() == UnaryExpr.Operator.POSTFIX_INCREMENT || u.getOperator() == UnaryExpr.Operator.PREFIX_INCREMENT;
    }

    if (e.isAssignExpr()) {
      AssignExpr ae = e.asAssignExpr();
      if (!ae.getTarget().isNameExpr()) return false;
      if (!ae.getTarget().asNameExpr().getNameAsString().equals(var)) return false;

      return ae.getOperator() == AssignExpr.Operator.PLUS
          && ae.getValue().isIntegerLiteralExpr()
          && ae.getValue().asIntegerLiteralExpr().getValue().equals("1");
    }

    return false;
  }

  private boolean indexOnlyUsedForArrayAccess(Statement body, String idxVar, String arrName) {
    if (body == null) return true;

    List<NameExpr> uses = body.findAll(NameExpr.class).stream()
        .filter(ne -> ne.getNameAsString().equals(idxVar))
        .toList();

    for (NameExpr ne : uses) {
      Optional<ArrayAccessExpr> aaeOpt = ne.findAncestor(ArrayAccessExpr.class);
      if (aaeOpt.isEmpty()) return false;
      ArrayAccessExpr aae = aaeOpt.get();

      if (!aae.getName().isNameExpr()) return false;
      if (!aae.getName().asNameExpr().getNameAsString().equals(arrName)) return false;

      if (!aae.getIndex().isNameExpr()) return false;
      if (!aae.getIndex().asNameExpr().getNameAsString().equals(idxVar)) return false;
    }

    return true;
  }

  private String guessLoopVarName(String arrName) {
    if (arrName == null || arrName.isBlank()) return "ls_item";
    if (arrName.endsWith("s") && arrName.length() > 1) return arrName.substring(0, arrName.length() - 1);
    return "ls_" + arrName;
  }

  private record ClassicForHeader(String idxVar, int startValue, BinaryExpr.Operator op, Expression right, Expression update) {}

  private ClassicForHeader parseClassicForHeader(ForStmt fs) {
    if (fs == null) return null;
    if (fs.getInitialization().size() != 1) return null;
    if (fs.getCompare().isEmpty()) return null;
    if (fs.getUpdate().size() != 1) return null;

    Expression init = fs.getInitialization().get(0);
    String idxVar;
    int start;

    if (init.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = init.asVariableDeclarationExpr();
      if (vde.getVariables().size() != 1) return null;
      VariableDeclarator v = vde.getVariable(0);
      if (!v.getInitializer().isPresent()) return null;
      if (!v.getInitializer().get().isIntegerLiteralExpr()) return null;
      idxVar = v.getNameAsString();
      start = Integer.parseInt(v.getInitializer().get().asIntegerLiteralExpr().getValue());
    } else if (init.isAssignExpr()) {
      AssignExpr ae = init.asAssignExpr();
      if (ae.getOperator() != AssignExpr.Operator.ASSIGN) return null;
      if (!ae.getTarget().isNameExpr()) return null;
      if (!ae.getValue().isIntegerLiteralExpr()) return null;
      idxVar = ae.getTarget().asNameExpr().getNameAsString();
      start = Integer.parseInt(ae.getValue().asIntegerLiteralExpr().getValue());
    } else {
      return null;
    }

    Expression cmp = fs.getCompare().get();
    if (cmp.isEnclosedExpr()) cmp = cmp.asEnclosedExpr().getInner();
    if (!cmp.isBinaryExpr()) return null;

    BinaryExpr b = cmp.asBinaryExpr();
    if (b.getOperator() != BinaryExpr.Operator.LESS && b.getOperator() != BinaryExpr.Operator.LESS_EQUALS) return null;
    if (!b.getLeft().isNameExpr()) return null;
    if (!b.getLeft().asNameExpr().getNameAsString().equals(idxVar)) return null;

    return new ClassicForHeader(idxVar, start, b.getOperator(), b.getRight(), fs.getUpdate().get(0));
  }

  // ===========================
  // Try / Switch
  // ===========================
  private void translateTryStmt(TryStmt ts, StringBuilder out, int indent, OutputStyle style) {
    emit(out, indent, "TRY.");
    for (Statement st : ts.getTryBlock().getStatements()) translateStatement(st, out, indent + 2, style);

    for (CatchClause cc : ts.getCatchClauses()) {
      String var = cc.getParameter().getNameAsString();
      emit(out, indent, "CATCH cx_root INTO DATA(" + var + ").");
      emit(out, indent + 2, "\" TODO: Java exception type nicht gemappt: " + cc.getParameter().getTypeAsString());
      for (Statement st : cc.getBody().getStatements()) translateStatement(st, out, indent + 2, style);
    }

    if (ts.getFinallyBlock().isPresent()) {
      emit(out, indent, "FINALLY.");
      for (Statement st : ts.getFinallyBlock().get().getStatements()) translateStatement(st, out, indent + 2, style);
    }

    emit(out, indent, "ENDTRY.");
  }

  private void translateSwitchStmt(SwitchStmt ss, StringBuilder out, int indent, OutputStyle style) {
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
        if (st.isBreakStmt()) continue;
        translateStatement(st, out, indent + 4, style);
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
  private void translateExpressionStmt(ExpressionStmt es, StringBuilder out, int indent, OutputStyle style) {
    Expression e = es.getExpression();
    emitLeadingComments(e, out, indent);

    if (e.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = e.asVariableDeclarationExpr();
      for (VariableDeclarator v : vde.getVariables()) {
        translateVarDeclarator(v, out, indent, style);
      }
      return;
    }

    if (e.isAssignExpr()) {
      translateAssignExpr(e.asAssignExpr(), out, indent, style);
      return;
    }

    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();

      if (isSystemOutPrintln(mc) || isSystemOutPrint(mc)) {
        Expression arg0 = !mc.getArguments().isEmpty() ? mc.getArgument(0) : null;
        String rendered = renderPrintArg(arg0); // handles "Hello," + x => |Hello,{ x }|
        if (style == OutputStyle.CLASSRUN_OUT) {
          emit(out, indent, "out->write( " + rendered + " ).");
        } else {
          emit(out, indent, "WRITE: / " + rendered + ".");
        }
        return;
      }

      if (mc.getArguments().isEmpty()) {
        emit(out, indent, abapCallNoArgs(mc) );
        return;
      }

      emitTodoJava(out, indent, es.toString(), "Method call mit Argumenten nicht eindeutig (ABAP braucht Parameternamen)");
      return;
    }

    if (e.isObjectCreationExpr()) {
      emitTodoJava(out, indent, es.toString(), "'new' als Statement ohne Zuweisung");
      return;
    }

    emitTodoJava(out, indent, es.toString(), "Expression nicht unterstützt / nicht eindeutig");
  }

  private String renderPrintArg(Expression e) {
    if (e == null) return "''";
    // if expr already becomes a string template, keep it; else wrap to template to avoid type quirks
    String x = expr(e);
    if (x.startsWith("|") && x.endsWith("|")) return x;
    if (e.isStringLiteralExpr() || e.isCharLiteralExpr()) return x;
    return "|{ " + x + " }|";
  }

  private void translateVarDeclarator(VariableDeclarator v, StringBuilder out, int indent, OutputStyle style) {
    String name = v.getNameAsString();
    String javaType = v.getTypeAsString();

    // ---- NEW: Java array initializer -> ABAP VALUE #( ... )
    if (v.getInitializer().isPresent()) {
      Expression init = v.getInitializer().get();

      ArrayInitializerExpr aie = null;
      if (init.isArrayInitializerExpr()) {
        aie = init.asArrayInitializerExpr();
      } else if (init.isArrayCreationExpr() && init.asArrayCreationExpr().getInitializer().isPresent()) {
        aie = init.asArrayCreationExpr().getInitializer().get();
      }

      if (aie != null) {
        emit(out, indent, "DATA " + name + " TYPE " + mapTypeForAbap(javaType) + ".");
        emit(out, indent, name + " = " + abapValueForArrayInitializer(aie) + ".");
        return;
      }
    }

    if (v.getInitializer().isPresent() && v.getInitializer().get().isObjectCreationExpr()) {
      ObjectCreationExpr oce = v.getInitializer().get().asObjectCreationExpr();
      emitNewVar(name, javaType, oce, out, indent);
      return;
    }

    emit(out, indent, "DATA " + name + " TYPE " + mapTypeForAbap(javaType) + ".");

    if (v.getInitializer().isPresent()) {
      Expression init = v.getInitializer().get();
      ExprRes er = tryExpr(init);
      if (er.ok) emit(out, indent, name + " = " + er.abap + ".");
      else emitTodoJava(out, indent, v.toString() + ";", er.reason);
    }
  }

  private String abapValueForArrayInitializer(ArrayInitializerExpr aie) {
    List<String> rows = new ArrayList<>();
    for (Expression e : aie.getValues()) rows.add("( " + expr(e) + " )");
    return "VALUE #( " + String.join(" ", rows) + " )";
  }

  private void translateAssignExpr(AssignExpr ae, StringBuilder out, int indent, OutputStyle style) {
    Expression targetExpr = ae.getTarget();
    Expression valueExpr = ae.getValue();

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
        emit(out, indent, "\" TODO: Java \"" + ae + "\" ist trickreich; Effekt: Wert bleibt gleich.");
        emit(out, indent, t + " = " + t + ".");
        return;
      }
    }

    if (ae.getOperator() == AssignExpr.Operator.ASSIGN && valueExpr.isObjectCreationExpr()) {
      emitNewAssign(expr(targetExpr), valueExpr.asObjectCreationExpr(), out, indent);
      return;
    }

    String target = expr(targetExpr);

    if (ae.getOperator() != AssignExpr.Operator.ASSIGN) {
      ExprRes rhs = tryExpr(valueExpr);
      if (!rhs.ok) {
        emitTodoJava(out, indent, ae + ";", rhs.reason);
        return;
      }

      switch (ae.getOperator()) {
        case PLUS -> emit(out, indent, target + " = " + target + " + " + rhs.abap + ".");
        case MINUS -> emit(out, indent, target + " = " + target + " - " + rhs.abap + ".");
        case MULTIPLY -> emit(out, indent, target + " = " + target + " * " + rhs.abap + ".");
        case DIVIDE -> emit(out, indent, target + " = " + target + " / " + rhs.abap + ".");
        default -> emitTodoJava(out, indent, ae + ";", "compound assignment nicht gemappt: " + ae.getOperator());
      }
      return;
    }

    ExprRes rhs = tryExpr(valueExpr);
    if (!rhs.ok) {
      emitTodoJava(out, indent, ae + ";", rhs.reason);
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

    // ---- NEW: Java array access arr[i] -> substitute OR itab[ i + 1 ]
    if (e.isArrayAccessExpr()) {
      ArrayAccessExpr aae = e.asArrayAccessExpr();

      if (aae.getName().isNameExpr() && aae.getIndex().isNameExpr()) {
        String arr = aae.getName().asNameExpr().getNameAsString();
        String idx = aae.getIndex().asNameExpr().getNameAsString();
        String key = arr + "#" + idx;
        if (arrayIndexSubst.containsKey(key)) return arrayIndexSubst.get(key);
      }

      String arr = expr(aae.getName());
      Expression idxE = aae.getIndex();
      if (idxE.isIntegerLiteralExpr()) {
        int i0 = Integer.parseInt(idxE.asIntegerLiteralExpr().getValue());
        return arr + "[ " + (i0 + 1) + " ]";
      }
      return arr + "[ " + expr(idxE) + " + 1 ]";
    }

    if (e.isFieldAccessExpr()) {
      FieldAccessExpr fa = e.asFieldAccessExpr();

      // ---- NEW: arr.length -> lines( arr )
      if (fa.getNameAsString().equals("length")) {
        return "lines( " + expr(fa.getScope()) + " )";
      }

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
    if (e.isCharLiteralExpr()) return "'" + String.valueOf(e.asCharLiteralExpr().asChar()).replace("'", "''") + "'";
    if (e.isEnclosedExpr()) return "(" + expr(e.asEnclosedExpr().getInner()) + ")";

    if (e.isBinaryExpr()) {
      BinaryExpr b = e.asBinaryExpr();

      // ---- NEW: Java string concat with '+' -> ABAP string template
      if (b.getOperator() == BinaryExpr.Operator.PLUS && isProbablyStringConcat(b)) {
        return abapStringTemplateFromPlus(b);
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
      throw new UnsupportedExpression("Unary ++/-- als Expression nicht sicher in ABAP übersetzbar");
    }

    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();

      if (isSystemOutPrintln(mc) || isSystemOutPrint(mc)) {
        throw new UnsupportedExpression("System.out.* als Expression nicht sinnvoll");
      }

      // ---- NEW: equals / equalsIgnoreCase
      if (mc.getNameAsString().equals("equals") && mc.getArguments().size() == 1 && mc.getScope().isPresent()) {
        return expr(mc.getScope().get()) + " = " + expr(mc.getArgument(0));
      }
      if (mc.getNameAsString().equals("equalsIgnoreCase") && mc.getArguments().size() == 1 && mc.getScope().isPresent()) {
        return "to_upper( " + expr(mc.getScope().get()) + " ) = to_upper( " + expr(mc.getArgument(0)) + " )";
      }

      // list.get(0) best-effort -> list[ 1 ]
      if (mc.getNameAsString().equals("get") && mc.getArguments().size() == 1 && mc.getScope().isPresent()) {
        Expression idx = mc.getArgument(0);
        if (idx.isIntegerLiteralExpr()) {
          int i0 = Integer.parseInt(idx.asIntegerLiteralExpr().getValue());
          int i1 = i0 + 1;
          String table = abapValue(mc.getScope().get());
          return table + "[ " + i1 + " ]";
        }
        String table = abapValue(mc.getScope().get());
        return table + "[ " + expr(idx) + " + 1 ]";
      }

      throw new UnsupportedExpression("Method call als Expression nicht eindeutig (ABAP Parameternamen/Tabellentyp fehlen)");
    }

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

  // ---- string concat helpers
  private boolean isProbablyStringConcat(Expression e) {
    return e.findFirst(StringLiteralExpr.class).isPresent() || e.findFirst(CharLiteralExpr.class).isPresent();
  }

  private String abapStringTemplateFromPlus(Expression e) {
    List<Expression> parts = new ArrayList<>();
    flattenPlus(e, parts);

    StringBuilder tpl = new StringBuilder();
    tpl.append("|");
    for (Expression p : parts) {
      if (p.isStringLiteralExpr()) {
        tpl.append(escapeForAbapTemplate(p.asStringLiteralExpr().asString()));
      } else if (p.isCharLiteralExpr()) {
        tpl.append(escapeForAbapTemplate(String.valueOf(p.asCharLiteralExpr().asChar())));
      } else {
        tpl.append("{ ").append(expr(p)).append(" }");
      }
    }
    tpl.append("|");
    return tpl.toString();
  }

  private void flattenPlus(Expression e, List<Expression> out) {
    if (e == null) return;
    if (e.isEnclosedExpr()) { flattenPlus(e.asEnclosedExpr().getInner(), out); return; }

    if (e.isBinaryExpr() && e.asBinaryExpr().getOperator() == BinaryExpr.Operator.PLUS) {
      BinaryExpr b = e.asBinaryExpr();
      flattenPlus(b.getLeft(), out);
      flattenPlus(b.getRight(), out);
    } else {
      out.add(e);
    }
  }

  private String escapeForAbapTemplate(String s) {
    if (s == null) return "";
    return s.replace("|", "||");
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
      emitTodoJava(out, indent, varName + " = " + oce + ";", "Konstruktor-Argumente: ABAP braucht Parameternamen");
      return;
    }

    emit(out, indent, varName + " = NEW " + abapClass + "( ).");
  }

  private void emitNewAssign(String target, ObjectCreationExpr oce, StringBuilder out, int indent) {
    String created = stripGeneric(oce.getTypeAsString());
    String abapClass = mapJavaTypeToAbapClass(created);

    if (!oce.getArguments().isEmpty()) {
      emitTodoJava(out, indent, target + " = " + oce + ";", "Konstruktor-Argumente: ABAP braucht Parameternamen");
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

  private String resolveVarTypeName(Node node, String varName) {
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

  private void emitLeadingComments(Node node, StringBuilder out, int indent) {
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
  // TODO output
  // ===========================
  private void emitTodoJava(StringBuilder out, int indent, String javaCode, String reason) {
    String j = (javaCode == null ? "" : javaCode).replace("\r\n", "\n").replace("\r", "\n").trim();
    if (j.isEmpty()) {
      emit(out, indent, "\" TODO: (leer) (weil: " + safe(reason) + ")");
      return;
    }

    String[] lines = j.split("\n");
    emit(out, indent, "\" TODO: " + lines[0].trim() + (reason == null ? "" : "  (weil: " + safe(reason) + ")"));
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

    if (tRaw.endsWith("[]")) {
      String elem = tRaw.substring(0, tRaw.length() - 2).trim();
      return "STANDARD TABLE OF " + mapTypeForAbap(elem) + " WITH EMPTY KEY";
    }

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
    arrayIndexSubst.clear();
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
