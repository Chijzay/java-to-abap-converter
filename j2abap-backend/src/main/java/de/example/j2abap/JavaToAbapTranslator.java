package de.example.j2abap;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.stmt.*;
import org.springframework.stereotype.Service;

import java.util.Locale;
import java.util.Optional;

@Service
public class JavaToAbapTranslator {

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
      for (Statement st : block.getStatements()) {
        translateStatement(st, out, 0);
      }
    } catch (Exception e) {
      out.append("Parse error (Java). Tipp: Snippet braucht gültige Statements (meist mit ;).").append("\n");
      out.append(shortMessage(e)).append("\n");
    }

    // ✅ HIER: formatieren (statt trim + "\n")
    return AbapFormatter.format(out.toString());
  }

  // ---------------------------
  // CLASS
  // ---------------------------
  private String translateClassInternal(String src) {
    StringBuilder out = new StringBuilder();

    try {
      CompilationUnit cu = StaticJavaParser.parse(src);

      Optional<ClassOrInterfaceDeclaration> clsOpt =
          cu.findFirst(ClassOrInterfaceDeclaration.class);

      if (clsOpt.isEmpty()) {
        return AbapFormatter.format("Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class.\n");
      }

      ClassOrInterfaceDeclaration cls = clsOpt.get();
      String javaName = cls.getNameAsString();
      String abapName = "zcl_" + toSnakeLower(javaName);

      // Definition
      out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");
      out.append("  PUBLIC SECTION.\n");

      for (MethodDeclaration md : cls.getMethods()) {
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

        if (md.getBody().isPresent()) {
          for (Statement st : md.getBody().get().getStatements()) {
            translateStatement(st, out, 2);
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

    // ✅ HIER: formatieren (statt return out.toString())
    return AbapFormatter.format(out.toString());
  }

  // ---------------------------
  // STATEMENTS (erweitert: Kommentare + while/for/foreach/switch)
  // ---------------------------
  private void translateStatement(Statement st, StringBuilder out, int indent) {
    if (st == null) return;

    // Java-Kommentare (z.B. // ... oder /* ... */) als ABAP-Kommentar-Zeile
    st.getComment().ifPresent(c -> {
      String txt = c.getContent() == null ? "" : c.getContent().trim();
      if (!txt.isBlank()) emit(out, indent, "\" " + txt.replace("\n", " "));
    });

    if (st.isBlockStmt()) {
      for (Statement inner : st.asBlockStmt().getStatements()) {
        translateStatement(inner, out, indent);
      }
      return;
    }

    if (st.isEmptyStmt()) return;

    // ----- Expressions -----
    if (st.isExpressionStmt()) {
      translateExpressionStmt(st.asExpressionStmt(), out, indent);
      return;
    }

    // ----- return -----
    if (st.isReturnStmt()) {
      ReturnStmt rs = st.asReturnStmt();
      rs.getExpression().ifPresent(expr -> emit(out, indent, "rv_result = " + expr(expr) + "."));
      emit(out, indent, "RETURN.");
      return;
    }

    // ----- if/else -----
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

    // ----- while -----
    if (st.isWhileStmt()) {
      WhileStmt ws = st.asWhileStmt();
      emit(out, indent, "WHILE " + expr(ws.getCondition()) + ".");
      translateStatement(ws.getBody(), out, indent + 2);
      emit(out, indent, "ENDWHILE.");
      return;
    }

    // ----- for (classic) -----
    if (st.isForStmt()) {
      ForStmt fs = st.asForStmt();

      // ABAP kann "DO ... TIMES" gut; wir nehmen das, wenn es nach i < N aussieht
      String times = tryExtractDoTimes(fs);
      if (times != null) {
        emit(out, indent, "DO " + times + " TIMES.");
        translateStatement(fs.getBody(), out, indent + 2);
        emit(out, indent, "ENDDO.");
        return;
      }

      // Fallback: deutlich markieren, aber nicht „falschen“ ABAP-Code vorspielen
      emit(out, indent, "\" TODO statement: For statement (manual rewrite needed)");
      return;
    }

    // ----- foreach (enhanced for) -----
    if (st.isForEachStmt()) {
      ForEachStmt fes = st.asForEachStmt();
      String var = fes.getVariable().getVariable(0).getNameAsString();
      String iterable = expr(fes.getIterable());
      emit(out, indent, "LOOP AT " + iterable + " INTO DATA(" + var + ").");
      translateStatement(fes.getBody(), out, indent + 2);
      emit(out, indent, "ENDLOOP.");
      return;
    }

    // ----- switch -----
    if (st.isSwitchStmt()) {
      SwitchStmt ss = st.asSwitchStmt();
      emit(out, indent, "CASE " + expr(ss.getSelector()) + ".");

      for (SwitchEntry entry : ss.getEntries()) {
        if (entry.getLabels().isEmpty()) {
          emit(out, indent + 2, "WHEN OTHERS.");
        } else {
          // Mehrere Labels -> mehrere WHEN-Zeilen
          for (Expression label : entry.getLabels()) {
            emit(out, indent + 2, "WHEN " + expr(label) + ".");
          }
        }

        for (Statement es : entry.getStatements()) {
          translateStatement(es, out, indent + 4);
        }
      }

      emit(out, indent, "ENDCASE.");
      return;
    }

    // Alles andere: TODO
    emit(out, indent, "\" TODO statement: " + prettyNodeName(st.getClass().getSimpleName()));
  }

  private void translateExpressionStmt(ExpressionStmt es, StringBuilder out, int indent) {
    Expression e = es.getExpression();

    // Kommentar direkt an Expression (z.B. x++; // ...)
    es.getComment().ifPresent(c -> {
      String txt = c.getContent() == null ? "" : c.getContent().trim();
      if (!txt.isBlank()) emit(out, indent, "\" " + txt.replace("\n", " "));
    });

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
  // EXPR (minimal)
  // ---------------------------
  private String expr(Expression e) {
    if (e == null) return "''";

    if (e.isNameExpr()) return e.asNameExpr().getNameAsString();
    if (e.isFieldAccessExpr()) return e.asFieldAccessExpr().getNameAsString();
    if (e.isIntegerLiteralExpr()) return e.asIntegerLiteralExpr().getValue();
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

  // Versuch: for(i=0; i<5; i++) -> DO 5 TIMES.
  private String tryExtractDoTimes(ForStmt fs) {
    try {
      if (fs.getCompare().isEmpty()) return null;
      Expression cmp = fs.getCompare().get();
      if (!cmp.isBinaryExpr()) return null;

      BinaryExpr be = cmp.asBinaryExpr();
      if (!(be.getOperator() == BinaryExpr.Operator.LESS || be.getOperator() == BinaryExpr.Operator.LESS_EQUALS)) return null;

      // rechts muss Literal sein
      Expression right = be.getRight();
      if (!right.isIntegerLiteralExpr()) return null;

      return right.asIntegerLiteralExpr().getValue();
    } catch (Exception ignored) {
      return null;
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
