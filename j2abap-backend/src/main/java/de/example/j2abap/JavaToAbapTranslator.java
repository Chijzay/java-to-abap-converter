package de.example.j2abap;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.comments.BlockComment;
import com.github.javaparser.ast.comments.Comment;
import com.github.javaparser.ast.comments.JavadocComment;
import com.github.javaparser.ast.comments.LineComment;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.stmt.*;
import org.springframework.stereotype.Service;

import java.util.Locale;
import java.util.Optional;

@Service
public class JavaToAbapTranslator {

  private enum Ctx { NORMAL, LOOP, SWITCH }

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

  // ---------------------------
  // SNIPPET
  // ---------------------------
  private String translateSnippetInternal(String src) {
    StringBuilder out = new StringBuilder();

    try {
      BlockStmt block = StaticJavaParser.parseBlock("{\n" + (src == null ? "" : src) + "\n}");

      // Orphan comments (falls vorhanden)
      for (Comment c : block.getOrphanComments()) {
        emitComment(out, 0, c);
      }

      for (Statement st : block.getStatements()) {
        translateStatement(st, out, 0, Ctx.NORMAL);
      }
    } catch (Exception e) {
      out.append("Parse error (Java). Tipp: Snippet braucht gültige Statements (meist mit ;).").append("\n");
      out.append(shortMessage(e)).append("\n");
    }

    return out.toString().trim() + "\n";
  }

  // ---------------------------
  // CLASS
  // ---------------------------
  private String translateClassInternal(String src) {
    StringBuilder out = new StringBuilder();

    try {
      CompilationUnit cu = StaticJavaParser.parse(src);

      // Top-level orphan comments (z.B. Datei-Kommentar)
      for (Comment c : cu.getOrphanComments()) {
        emitComment(out, 0, c);
      }

      Optional<ClassOrInterfaceDeclaration> clsOpt =
          cu.findFirst(ClassOrInterfaceDeclaration.class);

      if (clsOpt.isEmpty()) {
        return "Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class.\n";
      }

      ClassOrInterfaceDeclaration cls = clsOpt.get();
      String javaName = cls.getNameAsString();
      String abapName = "zcl_" + toSnakeLower(javaName);

      // Klassendeklarations-Kommentar (falls vorhanden)
      cls.getComment().ifPresent(c -> emitComment(out, 0, c));

      // Definition
      out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");
      out.append("  PUBLIC SECTION.\n");

      for (MethodDeclaration md : cls.getMethods()) {
        // Methoden-Kommentar in der Definition
        md.getComment().ifPresent(c -> emitComment(out, 4, c));

        out.append("    METHODS ").append(md.getNameAsString());

        if (!md.getParameters().isEmpty()) {
          out.append(" IMPORTING");
          md.getParameters().forEach(p ->
              out.append(" ").append(p.getNameAsString())
                 .append(" TYPE ").append(mapType(p.getTypeAsString()))
          );
        }

        if (!md.getType().isVoidType()) {
          out.append(" RETURNING VALUE(rv_result) TYPE ").append(mapType(md.getTypeAsString()));
        }

        out.append(".\n");
      }

      out.append("ENDCLASS.\n\n");

      // Implementation
      out.append("CLASS ").append(abapName).append(" IMPLEMENTATION.\n");

      for (MethodDeclaration md : cls.getMethods()) {
        out.append("  METHOD ").append(md.getNameAsString()).append(".\n");

        // Methoden-Kommentar in der Implementation
        md.getComment().ifPresent(c -> emitComment(out, 2, c));

        if (md.getBody().isPresent()) {
          BlockStmt body = md.getBody().get();

          // Orphan comments im Methodenblock
          for (Comment c : body.getOrphanComments()) {
            emitComment(out, 2, c);
          }

          for (Statement st : body.getStatements()) {
            translateStatement(st, out, 2, Ctx.NORMAL);
          }
        } else {
          emit(out, 2, "\" TODO statement: Method body missing");
        }

        out.append("  ENDMETHOD.\n\n");
      }

      out.append("ENDCLASS.\n");

    } catch (Exception e) {
      out.append("Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class.\n");
      out.append(shortMessage(e)).append("\n");
    }

    return out.toString();
  }

  // ---------------------------
  // STATEMENTS
  // ---------------------------
  private void translateStatement(Statement st, StringBuilder out, int indent, Ctx ctx) {
    if (st == null) return;

    // Kommentar direkt am Statement
    st.getComment().ifPresent(c -> emitComment(out, indent, c));

    if (st.isBlockStmt()) {
      BlockStmt b = st.asBlockStmt();
      for (Comment c : b.getOrphanComments()) emitComment(out, indent, c);
      for (Statement inner : b.getStatements()) translateStatement(inner, out, indent, ctx);
      return;
    }

    if (st.isEmptyStmt()) return;

    if (st.isExpressionStmt()) {
      translateExpressionAsStatement(st.asExpressionStmt().getExpression(), out, indent, ctx);
      return;
    }

    if (st.isReturnStmt()) {
      ReturnStmt rs = st.asReturnStmt();
      rs.getExpression().ifPresent(expr -> emit(out, indent, "rv_result = " + expr(expr) + "."));
      emit(out, indent, "RETURN.");
      return;
    }

    if (st.isIfStmt()) {
      IfStmt is = st.asIfStmt();
      emit(out, indent, "IF " + expr(is.getCondition()) + ".");
      translateStatement(is.getThenStmt(), out, indent + 2, ctx);
      if (is.getElseStmt().isPresent()) {
        emit(out, indent, "ELSE.");
        translateStatement(is.getElseStmt().get(), out, indent + 2, ctx);
      }
      emit(out, indent, "ENDIF.");
      return;
    }

    // while
    if (st.isWhileStmt()) {
      WhileStmt ws = st.asWhileStmt();
      emit(out, indent, "WHILE " + expr(ws.getCondition()) + ".");
      translateStatement(ws.getBody(), out, indent + 2, Ctx.LOOP);
      emit(out, indent, "ENDWHILE.");
      return;
    }

    // do/while
    if (st.isDoStmt()) {
      DoStmt ds = st.asDoStmt();
      emit(out, indent, "DO.");
      translateStatement(ds.getBody(), out, indent + 2, Ctx.LOOP);
      emit(out, indent + 2, "IF NOT ( " + expr(ds.getCondition()) + " ).");
      emit(out, indent + 4, "EXIT.");
      emit(out, indent + 2, "ENDIF.");
      emit(out, indent, "ENDDO.");
      return;
    }

    // for
    if (st.isForStmt()) {
      translateForStmt(st.asForStmt(), out, indent);
      return;
    }

    // foreach
    if (st.isForEachStmt()) {
      translateForEachStmt(st.asForEachStmt(), out, indent);
      return;
    }

    // switch
    if (st.isSwitchStmt()) {
      translateSwitchStmt(st.asSwitchStmt(), out, indent);
      return;
    }

    // break / continue
    if (st.isBreakStmt()) {
      if (ctx == Ctx.SWITCH) {
        // ABAP CASE braucht kein break -> ignorieren
        return;
      }
      if (ctx == Ctx.LOOP) {
        emit(out, indent, "EXIT.");
      } else {
        emit(out, indent, "\" TODO statement: break (only valid in loop/switch)");
      }
      return;
    }

    if (st.isContinueStmt()) {
      if (ctx == Ctx.LOOP) {
        emit(out, indent, "CONTINUE.");
      } else {
        emit(out, indent, "\" TODO statement: continue (only valid in loops)");
      }
      return;
    }

    // Alles andere: TODO
    emit(out, indent, "\" TODO statement: " + prettyNodeName(st.getClass().getSimpleName()));
  }

  private void translateForStmt(ForStmt fs, StringBuilder out, int indent) {
    // Init (z.B. int i=0;)
    for (Expression init : fs.getInitialization()) {
      translateExpressionAsStatement(init, out, indent, Ctx.NORMAL);
    }

    // Bedingung -> WHILE. Wenn keine Bedingung: DO + Exit TODO
    if (fs.getCompare().isPresent()) {
      emit(out, indent, "WHILE " + expr(fs.getCompare().get()) + ".");
      translateStatement(fs.getBody(), out, indent + 2, Ctx.LOOP);

      // Update (z.B. i++)
      for (Expression upd : fs.getUpdate()) {
        translateExpressionAsStatement(upd, out, indent + 2, Ctx.LOOP);
      }

      emit(out, indent, "ENDWHILE.");
    } else {
      emit(out, indent, "DO.");
      emit(out, indent + 2, "\" TODO statement: for-loop without compare; add EXIT condition");
      translateStatement(fs.getBody(), out, indent + 2, Ctx.LOOP);

      for (Expression upd : fs.getUpdate()) {
        translateExpressionAsStatement(upd, out, indent + 2, Ctx.LOOP);
      }

      emit(out, indent, "ENDDO.");
    }
  }

  private void translateForEachStmt(ForEachStmt fe, StringBuilder out, int indent) {
    // Kommentare am foreach
    fe.getComment().ifPresent(c -> emitComment(out, indent, c));

    String varName = fe.getVariable().getVariable(0).getNameAsString();
    String iterable = expr(fe.getIterable());

    // ABAP: LOOP AT itab INTO DATA(var).
    emit(out, indent, "LOOP AT " + iterable + " INTO DATA(" + varName + ").");
    translateStatement(fe.getBody(), out, indent + 2, Ctx.LOOP);
    emit(out, indent, "ENDLOOP.");
  }

  private void translateSwitchStmt(SwitchStmt ss, StringBuilder out, int indent) {
    ss.getComment().ifPresent(c -> emitComment(out, indent, c));

    emit(out, indent, "CASE " + expr(ss.getSelector()) + ".");

    for (SwitchEntry entry : ss.getEntries()) {
      // Entry Kommentar
      entry.getComment().ifPresent(c -> emitComment(out, indent + 2, c));

      if (entry.getLabels().isEmpty()) {
        emit(out, indent + 2, "WHEN OTHERS.");
      } else {
        // Mehrere Labels -> WHEN a OR b OR c.
        StringBuilder when = new StringBuilder();
        when.append("WHEN ");
        for (int i = 0; i < entry.getLabels().size(); i++) {
          if (i > 0) when.append(" OR ");
          when.append(expr(entry.getLabels().get(i)));
        }
        when.append(".");
        emit(out, indent + 2, when.toString());
      }

      // Statements im Case
      for (Statement st : entry.getStatements()) {
        translateStatement(st, out, indent + 4, Ctx.SWITCH);
      }
    }

    emit(out, indent, "ENDCASE.");
  }

  // ---------------------------
  // EXPRESSION as Statement
  // ---------------------------
  private void translateExpressionAsStatement(Expression e, StringBuilder out, int indent, Ctx ctx) {
    if (e == null) return;

    // Kommentar am Expression-Node
    e.getComment().ifPresent(c -> emitComment(out, indent, c));

    if (e.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = e.asVariableDeclarationExpr();
      vde.getVariables().forEach(v -> {
        String name = v.getNameAsString();
        String type = mapType(v.getTypeAsString());
        emit(out, indent, "DATA " + name + " TYPE " + type + ".");
        v.getInitializer().ifPresent(init -> emit(out, indent, name + " = " + expr(init) + "."));
      });
      return;
    }

    if (e.isAssignExpr()) {
      AssignExpr ae = e.asAssignExpr();
      emit(out, indent, expr(ae.getTarget()) + " = " + expr(ae.getValue()) + ".");
      return;
    }

    // ++i / i++ / --i / i--
    if (e.isUnaryExpr()) {
      UnaryExpr u = e.asUnaryExpr();
      UnaryExpr.Operator op = u.getOperator();
      String target = expr(u.getExpression());

      if (op == UnaryExpr.Operator.POSTFIX_INCREMENT || op == UnaryExpr.Operator.PREFIX_INCREMENT) {
        emit(out, indent, target + " = " + target + " + 1.");
        return;
      }
      if (op == UnaryExpr.Operator.POSTFIX_DECREMENT || op == UnaryExpr.Operator.PREFIX_DECREMENT) {
        emit(out, indent, target + " = " + target + " - 1.");
        return;
      }

      emit(out, indent, "\" TODO expression: " + prettyNodeName(u.getClass().getSimpleName()));
      return;
    }

    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();
      if (isSystemOutPrintln(mc)) {
        String arg = mc.getArguments().isNonEmpty() ? expr(mc.getArgument(0)) : "''";
        emit(out, indent, "WRITE: / " + arg + ".");
        return;
      }
      emit(out, indent, "\" TODO expression: " + prettyNodeName(mc.getClass().getSimpleName()));
      return;
    }

    emit(out, indent, "\" TODO expression: " + prettyNodeName(e.getClass().getSimpleName()));
  }

  // ---------------------------
  // EXPR
  // ---------------------------
  private String expr(Expression e) {
    if (e == null) return "''";

    if (e.isNameExpr()) return e.asNameExpr().getNameAsString();
    if (e.isFieldAccessExpr()) return e.asFieldAccessExpr().getNameAsString();
    if (e.isIntegerLiteralExpr()) return e.asIntegerLiteralExpr().getValue();
    if (e.isLongLiteralExpr()) return e.asLongLiteralExpr().getValue().replace("L", "");
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
        return "NOT (" + expr(u.getExpression()) + ")";
      }
      return expr(u.getExpression());
    }

    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();
      if (isSystemOutPrintln(mc)) return "''";
      return mc.getNameAsString() + "( ... )";
    }

    // Fallback: lieber neutraler Placeholder als kaputter ABAP-Ausdruck
    return "''";
  }

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

  // ---------------------------
  // COMMENTS: Java -> ABAP (")
  // ---------------------------
  private static void emitComment(StringBuilder out, int indent, Comment c) {
    if (c == null) return;

    String content = c.getContent();
    if (content == null) content = "";

    // Javadoc/Block oft mit "*" am Zeilenanfang -> sauber machen
    String[] lines = content.split("\\R", -1);
    if (lines.length == 0) {
      emit(out, indent, "\"");
      return;
    }

    // Typ-Hinweis (optional, aber hilft beim Lernen)
    if (c instanceof JavadocComment) {
      emit(out, indent, "\" JavaDoc:");
    }

    for (String line : lines) {
      String cleaned = line
          .replaceFirst("^\\s*\\*\\s?", "")   // leading "* "
          .stripTrailing();

      if (cleaned.isBlank()) {
        emit(out, indent, "\"");
      } else {
        emit(out, indent, "\" " + cleaned);
      }
    }

    // LineComment hat meist nur eine Zeile; Block/Javadoc kann mehrzeilig sein
    if (c instanceof LineComment) {
      // nichts extra
    } else if (c instanceof BlockComment || c instanceof JavadocComment) {
      // optische Trennung nicht erzwingen
    }
  }

  // ---------------------------
  // Helpers
  // ---------------------------
  private static void emit(StringBuilder out, int indent, String line) {
    out.append(" ".repeat(Math.max(0, indent))).append(line).append("\n");
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

  private String prettyNodeName(String simple) {
    if (simple == null) return "Statement";
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
}
