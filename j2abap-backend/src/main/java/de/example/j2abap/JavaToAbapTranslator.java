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

    return ensureTrailingNewline(out);
  }

  // ---------------------------
  // CLASS
  // ---------------------------
  private String translateClassInternal(String src) {
    StringBuilder out = new StringBuilder();

    try {
      CompilationUnit cu = StaticJavaParser.parse(src);

      // Optional: file-level comments
      emitCompilationUnitHeaderComments(cu, out);

      Optional<ClassOrInterfaceDeclaration> clsOpt = cu.findFirst(ClassOrInterfaceDeclaration.class);
      if (clsOpt.isEmpty()) {
        // If it's an enum file, still handle
        Optional<EnumDeclaration> enumOpt = cu.findFirst(EnumDeclaration.class);
        if (enumOpt.isPresent()) {
          translateEnumDeclaration(enumOpt.get(), out, 0);
          return ensureTrailingNewline(out);
        }
        return "Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/enum.\n";
      }

      ClassOrInterfaceDeclaration cls = clsOpt.get();
      String javaName = cls.getNameAsString();
      String abapName = "zcl_" + toSnakeLower(javaName);

      // Class comment (javadoc / comments)
      emitLeadingComments(cls, out, 0);

      // Definition
      out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");
      out.append("  PUBLIC SECTION.\n");

      // Fields -> DATA declarations
      for (FieldDeclaration fd : cls.getFields()) {
        emitLeadingComments(fd, out, 4);
        for (VariableDeclarator v : fd.getVariables()) {
          String name = v.getNameAsString();
          String type = mapType(v.getTypeAsString());
          // Qualifiers hint
          String hint = fieldModifiersHint(fd);
          if (!hint.isEmpty()) emit(out, 4, "\" " + hint);
          emit(out, 4, "DATA " + name + " TYPE " + type + ".");
          v.getInitializer().ifPresent(init -> emit(out, 4, name + " = " + expr(init) + "."));
        }
      }

      // Enums inside class (rare, but supported)
      for (EnumDeclaration ed : cls.findAll(EnumDeclaration.class)) {
        emit(out, 4, "\" Enum detected (hint): " + ed.getNameAsString() + " = " +
            ed.getEntries().stream().map(EnumConstantDeclaration::getNameAsString).collect(Collectors.joining(", ")));
      }

      // Methods signatures
      for (MethodDeclaration md : cls.getMethods()) {
        emitLeadingComments(md, out, 4);

        String hint = methodModifiersHint(md);
        if (!hint.isEmpty()) emit(out, 4, "\" " + hint);

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
          BlockStmt body = md.getBody().get();
          emitLeadingComments(body, out, 2);
          for (Statement st : body.getStatements()) {
            translateStatement(st, out, 2);
          }
        } else {
          emit(out, 2, "\" TODO statement: Method body missing");
        }

        out.append("  ENDMETHOD.\n\n");
      }

      out.append("ENDCLASS.\n");

    } catch (Exception e) {
      out.append("Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/enum.\n");
      out.append(shortMessage(e)).append("\n");
    }

    return ensureTrailingNewline(out);
  }

  // ---------------------------
  // STATEMENTS
  // ---------------------------
  private void translateStatement(Statement st, StringBuilder out, int indent) {
    if (st == null) return;

    emitLeadingComments(st, out, indent);

    if (st.isBlockStmt()) {
      for (Statement inner : st.asBlockStmt().getStatements()) {
        translateStatement(inner, out, indent);
      }
      return;
    }

    if (st.isEmptyStmt()) return;

    if (st.isExpressionStmt()) {
      translateExpressionStmt(st.asExpressionStmt(), out, indent);
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

    if (st.isForStmt()) {
      // Java for(...) -> ABAP doesn't have direct equivalent; we try:
      // - If looks like i=0; i<...; i++ -> DO ... TIMES + index var
      // - else: emit TODO skeleton
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

    if (st.isBreakStmt()) {
      // In ABAP CASE there is no break; in loops you can use EXIT.
      emit(out, indent, "EXIT.");
      return;
    }

    if (st.isContinueStmt()) {
      emit(out, indent, "CONTINUE.");
      return;
    }

    // Alles andere: TODO
    emit(out, indent, "\" TODO statement: " + prettyNodeName(st.getClass().getSimpleName()));
  }

  private void translateExpressionStmt(ExpressionStmt es, StringBuilder out, int indent) {
    Expression e = es.getExpression();

    emitLeadingComments(e, out, indent);

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

    if (e.isUnaryExpr()) {
      // i++ / ++i / i-- / --i
      UnaryExpr u = e.asUnaryExpr();
      if (u.getOperator() == UnaryExpr.Operator.POSTFIX_INCREMENT ||
          u.getOperator() == UnaryExpr.Operator.PREFIX_INCREMENT) {
        emit(out, indent, expr(u.getExpression()) + " = " + expr(u.getExpression()) + " + 1.");
        return;
      }
      if (u.getOperator() == UnaryExpr.Operator.POSTFIX_DECREMENT ||
          u.getOperator() == UnaryExpr.Operator.PREFIX_DECREMENT) {
        emit(out, indent, expr(u.getExpression()) + " = " + expr(u.getExpression()) + " - 1.");
        return;
      }
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

    if (e.isObjectCreationExpr()) {
      // new X(...) -> ABAP has CREATE OBJECT, but we don't know the type mapping.
      ObjectCreationExpr oce = e.asObjectCreationExpr();
      emit(out, indent, "\" TODO: new " + oce.getTypeAsString() + "(...)");
      return;
    }

    emit(out, indent, "\" TODO expression: " + prettyNodeName(e.getClass().getSimpleName()));
  }

  // ---------------------------
  // SWITCH
  // ---------------------------
  private void translateSwitchStmt(SwitchStmt ss, StringBuilder out, int indent) {
    // Java:
    // switch (x) { case A: ... break; case B: case C: ... break; default: ... }
    // ABAP:
    // CASE x. WHEN A. ... WHEN B OR C. ... WHEN OTHERS. ... ENDCASE.
    String selector = expr(ss.getSelector());
    emit(out, indent, "CASE " + selector + ".");

    // Collect fall-through labels: In old-style switch, multiple case labels can lead to same statements.
    // We'll group consecutive entries with no statements until the first entry with statements.
    List<SwitchEntry> entries = ss.getEntries();
    int i = 0;
    while (i < entries.size()) {
      SwitchEntry entry = entries.get(i);

      // Determine labels group for this block
      List<String> labels = new ArrayList<>();
      boolean isDefault = entry.getLabels().isEmpty();

      if (isDefault) {
        // default
      } else {
        labels.addAll(entry.getLabels().stream().map(this::switchLabelExpr).toList());
      }

      // Find statements for this block: if current entry has no statements, fall-through gather next
      List<Statement> stmts = new ArrayList<>(entry.getStatements());
      int j = i;
      while (stmts.isEmpty() && j + 1 < entries.size()) {
        SwitchEntry next = entries.get(j + 1);
        if (!next.getLabels().isEmpty()) {
          labels.addAll(next.getLabels().stream().map(this::switchLabelExpr).toList());
        } else {
          isDefault = true;
        }
        stmts = new ArrayList<>(next.getStatements());
        j++;
        if (!stmts.isEmpty()) break;
      }

      // Emit WHEN line
      if (isDefault) {
        emit(out, indent, "  WHEN OTHERS.");
      } else if (labels.isEmpty()) {
        emit(out, indent, "  WHEN OTHERS.");
      } else {
        String when = String.join(" OR ", labels);
        emit(out, indent, "  WHEN " + when + ".");
      }

      // Emit body statements; ignore "break;" (and map to nothing)
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
    // Enum constant MONDAY -> 'MONDAY' as placeholder
    if (e.isNameExpr()) return "'" + e.asNameExpr().getNameAsString() + "'";
    if (e.isFieldAccessExpr()) return "'" + e.asFieldAccessExpr().getNameAsString() + "'";
    if (e.isStringLiteralExpr()) return "'" + e.asStringLiteralExpr().asString().replace("'", "''") + "'";
    if (e.isCharLiteralExpr()) return "'" + e.asCharLiteralExpr().asChar() + "'";
    if (e.isIntegerLiteralExpr()) return e.asIntegerLiteralExpr().getValue();
    if (e.isBooleanLiteralExpr()) return e.asBooleanLiteralExpr().getValue() ? "abap_true" : "abap_false";
    // fallback
    return "'" + prettyNodeName(e.getClass().getSimpleName()).replace(" ", "_").toUpperCase(Locale.ROOT) + "'";
  }

  // ---------------------------
  // FOR / FOREACH
  // ---------------------------
  private void translateForStmt(ForStmt fs, StringBuilder out, int indent) {
    // Try to detect: for (int i=0; i < N; i++)  => DO N TIMES. i = sy-index - 1.
    // or for (i=0; i < N; i++) same.
    // If can't detect, emit TODO skeleton + translate body.
    Optional<ForLoopPattern> pat = detectSimpleCountingLoop(fs);
    if (pat.isPresent()) {
      ForLoopPattern p = pat.get();
      // Ensure counter var exists
      emit(out, indent, "\" for-loop mapped to DO...TIMES (best-effort)");
      emit(out, indent, "DO " + p.timesExpr + " TIMES.");
      // sy-index starts at 1
      emit(out, indent + 2, p.varName + " = sy-index - 1.");
      translateStatement(fs.getBody(), out, indent + 2);
      emit(out, indent, "ENDDO.");
      return;
    }

    emit(out, indent, "\" TODO statement: for-loop (non-trivial). Manual rewrite needed.");
    // Still translate body to show intent
    translateStatement(fs.getBody(), out, indent);
  }

  private void translateForEachStmt(ForEachStmt fes, StringBuilder out, int indent) {
    // for (T x : list) { ... } => LOOP AT list INTO x. ... ENDLOOP.
    String var = fes.getVariable().getVariable(0).getNameAsString();
    String iterable = expr(fes.getIterable());

    emit(out, indent, "\" foreach mapped to LOOP AT (best-effort)");
    emit(out, indent, "LOOP AT " + iterable + " INTO " + var + ".");
    translateStatement(fes.getBody(), out, indent + 2);
    emit(out, indent, "ENDLOOP.");
  }

  private record ForLoopPattern(String varName, String timesExpr) {}

  private Optional<ForLoopPattern> detectSimpleCountingLoop(ForStmt fs) {
    // init: int i = 0 OR i = 0
    // compare: i < N OR i <= N (we handle < only properly)
    // update: i++ / ++i / i += 1
    if (fs.getInitialization().size() != 1) return Optional.empty();
    if (fs.getCompare().isEmpty()) return Optional.empty();
    if (fs.getUpdate().size() != 1) return Optional.empty();

    String varName = null;

    Expression init = fs.getInitialization().get(0);
    if (init.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = init.asVariableDeclarationExpr();
      if (vde.getVariables().size() != 1) return Optional.empty();
      VariableDeclarator vd = vde.getVariable(0);
      if (vd.getInitializer().isEmpty()) return Optional.empty();
      if (!isZeroLiteral(vd.getInitializer().get())) return Optional.empty();
      varName = vd.getNameAsString();
    } else if (init.isAssignExpr()) {
      AssignExpr ae = init.asAssignExpr();
      if (!isZeroLiteral(ae.getValue())) return Optional.empty();
      varName = simpleName(ae.getTarget()).orElse(null);
    } else {
      return Optional.empty();
    }

    Expression cmp = fs.getCompare().get();
    if (!(cmp instanceof BinaryExpr b)) return Optional.empty();
    if (b.getOperator() != BinaryExpr.Operator.LESS) return Optional.empty();
    String left = simpleName(b.getLeft()).orElse(null);
    if (left == null || !left.equals(varName)) return Optional.empty();
    String timesExpr = expr(b.getRight());

    Expression upd = fs.getUpdate().get(0);
    boolean okUpdate = false;

    if (upd.isUnaryExpr()) {
      UnaryExpr u = upd.asUnaryExpr();
      if ((u.getOperator() == UnaryExpr.Operator.POSTFIX_INCREMENT ||
          u.getOperator() == UnaryExpr.Operator.PREFIX_INCREMENT) &&
          simpleName(u.getExpression()).map(varName::equals).orElse(false)) {
        okUpdate = true;
      }
    } else if (upd.isAssignExpr()) {
      AssignExpr ae = upd.asAssignExpr();
      if (simpleName(ae.getTarget()).map(varName::equals).orElse(false)) {
        if (ae.getOperator() == AssignExpr.Operator.PLUS) {
          okUpdate = isOneLiteral(ae.getValue());
        } else if (ae.getOperator() == AssignExpr.Operator.ASSIGN) {
          // i = i + 1
          if (ae.getValue().isBinaryExpr()) {
            BinaryExpr bb = ae.getValue().asBinaryExpr();
            if (bb.getOperator() == BinaryExpr.Operator.PLUS &&
                simpleName(bb.getLeft()).map(varName::equals).orElse(false) &&
                isOneLiteral(bb.getRight())) {
              okUpdate = true;
            }
          }
        }
      }
    }

    if (!okUpdate) return Optional.empty();
    return Optional.of(new ForLoopPattern(varName, timesExpr));
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

  // ---------------------------
  // EXPR
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
      // ++/-- handled in statement layer; here just pass through
      return expr(u.getExpression());
    }

    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();
      if (isSystemOutPrintln(mc)) return "''";
      return mc.getNameAsString() + "( ... )";
    }

    if (e.isConditionalExpr()) {
      // a ? b : c -> ABAP has COND #( ), but keep simple
      return "COND #( WHEN " + expr(e.asConditionalExpr().getCondition()) +
          " THEN " + expr(e.asConditionalExpr().getThenExpr()) +
          " ELSE " + expr(e.asConditionalExpr().getElseExpr()) + " )";
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

  // ---------------------------
  // ENUM (basic hint)
  // ---------------------------
  private void translateEnumDeclaration(EnumDeclaration ed, StringBuilder out, int indent) {
    emitLeadingComments(ed, out, indent);
    String name = ed.getNameAsString();
    String values = ed.getEntries().stream().map(EnumConstantDeclaration::getNameAsString).collect(Collectors.joining(", "));
    emit(out, indent, "\" Enum detected: " + name + " = " + values);
    emit(out, indent, "\" Hint: model this as a DOMAIN / constants or CHAR field in ABAP");
  }

  // ---------------------------
  // COMMENTS (Java -> ABAP)
  // ---------------------------
  private void emitCompilationUnitHeaderComments(CompilationUnit cu, StringBuilder out) {
    if (cu == null) return;
    cu.getComment().ifPresent(c -> emitComment(c, out, 0));
  }

  private void emitLeadingComments(com.github.javaparser.ast.Node node, StringBuilder out, int indent) {
    if (node == null) return;
    // Some comments are attached as "comment" directly
    node.getComment().ifPresent(c -> emitComment(c, out, indent));
  }

  private void emitComment(Comment c, StringBuilder out, int indent) {
    if (c == null) return;

    String raw = c.getContent();
    if (raw == null) raw = "";

    // Normalize linebreaks
    raw = raw.replace("\r\n", "\n").replace("\r", "\n").trim();

    // Javadoc/Block/Line -> ABAP comment style
    if (c instanceof LineComment) {
      emit(out, indent, "\" " + raw.trim());
      return;
    }

    if (c instanceof BlockComment || c instanceof JavadocComment) {
      String[] lines = raw.split("\n");
      for (String line : lines) {
        String t = line.strip();
        if (!t.isEmpty()) emit(out, indent, "\" " + t);
      }
    }
  }

  // ---------------------------
  // MODIFIER HINTS
  // ---------------------------
  private String methodModifiersHint(MethodDeclaration md) {
    if (md == null) return "";
    List<String> bits = new ArrayList<>();
    if (md.isPublic()) bits.add("public");
    if (md.isPrivate()) bits.add("private");
    if (md.isProtected()) bits.add("protected");
    if (md.isStatic()) bits.add("static (ABAP: use CLASS-METHODS if needed)");
    if (md.isFinal()) bits.add("final");
    if (bits.isEmpty()) return "";
    return "Java modifiers: " + String.join(", ", bits);
  }

  private String fieldModifiersHint(FieldDeclaration fd) {
    if (fd == null) return "";
    List<String> bits = new ArrayList<>();
    if (fd.isPublic()) bits.add("public");
    if (fd.isPrivate()) bits.add("private");
    if (fd.isProtected()) bits.add("protected");
    if (fd.isStatic()) bits.add("static (ABAP: use CLASS-DATA if needed)");
    if (fd.isFinal()) bits.add("final (ABAP: constants?)");
    if (bits.isEmpty()) return "";
    return "Java modifiers: " + String.join(", ", bits);
  }

  // ---------------------------
  // Helpers
  // ---------------------------
  private static void emit(StringBuilder out, int indent, String line) {
    out.append(" ".repeat(Math.max(0, indent))).append(line).append("\n");
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
