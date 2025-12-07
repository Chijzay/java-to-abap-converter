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
  // CLASS / INTERFACE / RECORD / ENUM
  // ---------------------------
  private String translateClassInternal(String src) {
    StringBuilder out = new StringBuilder();

    try {
      CompilationUnit cu = StaticJavaParser.parse(src);

      emitCompilationUnitHeaderComments(cu, out);

      // Top-level enum?
      Optional<EnumDeclaration> enumOpt = cu.findFirst(EnumDeclaration.class);
      Optional<ClassOrInterfaceDeclaration> clsOpt = cu.findFirst(ClassOrInterfaceDeclaration.class);
      Optional<RecordDeclaration> recOpt = cu.findFirst(RecordDeclaration.class);

      if (clsOpt.isEmpty() && recOpt.isEmpty()) {
        if (enumOpt.isPresent()) {
          translateEnumDeclaration(enumOpt.get(), out, 0);
          return ensureTrailingNewline(out);
        }
        return "Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/interface/record/enum.\n";
      }

      // INTERFACE
      if (clsOpt.isPresent() && clsOpt.get().isInterface()) {
        ClassOrInterfaceDeclaration itf = clsOpt.get();
        String javaName = itf.getNameAsString();
        String abapName = "zif_" + toSnakeLower(javaName);

        emitLeadingComments(itf, out, 0);

        out.append("INTERFACE ").append(abapName).append(" PUBLIC.\n");

        // method signatures
        for (MethodDeclaration md : itf.getMethods()) {
          emitLeadingComments(md, out, 2);

          String hint = methodModifiersHint(md);
          if (!hint.isEmpty()) emit(out, 2, "\" " + hint);

          out.append("  METHODS ").append(md.getNameAsString());

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

        emit(out, 2, "\" TODO: Interface constants / default methods are not mapped 1:1");
        out.append("ENDINTERFACE.\n");

        return ensureTrailingNewline(out);
      }

      // RECORD -> treat like class with readonly components + constructor hint
      if (recOpt.isPresent()) {
        RecordDeclaration rec = recOpt.get();
        String javaName = rec.getNameAsString();
        String abapName = "zcl_" + toSnakeLower(javaName);

        emitLeadingComments(rec, out, 0);

        out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");
        out.append("  PUBLIC SECTION.\n");

        emit(out, 4, "\" record mapped best-effort: components -> READ-ONLY attributes");
        for (Parameter p : rec.getParameters()) {
          emit(out, 4, "DATA " + p.getNameAsString() + " TYPE " + mapType(p.getTypeAsString()) + " READ-ONLY.");
        }

        // constructor (ABAP has one CONSTRUCTOR, Java records have canonical ctor)
        out.append("    METHODS constructor");
        if (!rec.getParameters().isEmpty()) {
          out.append(" IMPORTING");
          for (Parameter p : rec.getParameters()) {
            out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(mapType(p.getTypeAsString()));
          }
        }
        out.append(".\n");

        out.append("ENDCLASS.\n\n");

        out.append("CLASS ").append(abapName).append(" IMPLEMENTATION.\n");
        out.append("  METHOD constructor.\n");
        emit(out, 2, "\" TODO: assign imported params to attributes (names likely match, but verify)");
        for (Parameter p : rec.getParameters()) {
          emit(out, 2, "me->" + p.getNameAsString() + " = " + p.getNameAsString() + ".");
        }
        out.append("  ENDMETHOD.\n");
        out.append("ENDCLASS.\n");

        return ensureTrailingNewline(out);
      }

      // CLASS
      ClassOrInterfaceDeclaration cls = clsOpt.get();
      String javaName = cls.getNameAsString();
      String abapName = "zcl_" + toSnakeLower(javaName);

      emitLeadingComments(cls, out, 0);

      out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");

      // Group members by visibility section
      Map<Section, List<FieldDeclaration>> fieldsBySec = new EnumMap<>(Section.class);
      Map<Section, List<CallableDeclaration<?>>> methodsBySec = new EnumMap<>(Section.class);
      for (Section s : Section.values()) {
        fieldsBySec.put(s, new ArrayList<>());
        methodsBySec.put(s, new ArrayList<>());
      }

      for (FieldDeclaration fd : cls.getFields()) {
        fieldsBySec.get(sectionOf(fd)).add(fd);
      }

      // Constructors
      List<ConstructorDeclaration> ctors = cls.getConstructors();
      if (ctors.size() > 1) {
        // ABAP: no overload; we keep first, hint others
        emit(out, 0, "\" TODO: Multiple Java constructors detected; ABAP has only one CONSTRUCTOR (no overload).");
      }

      // Methods
      for (MethodDeclaration md : cls.getMethods()) {
        methodsBySec.get(sectionOf(md)).add(md);
      }

      // Add first constructor to its section
      if (!ctors.isEmpty()) {
        methodsBySec.get(sectionOf(ctors.get(0))).add(ctors.get(0));
      }

      // Initializers (static/instance blocks) -> comment
      List<InitializerDeclaration> inits = cls.getMembers().stream()
          .filter(m -> m instanceof InitializerDeclaration)
          .map(m -> (InitializerDeclaration) m)
          .toList();
      if (!inits.isEmpty()) {
        emit(out, 0, "\" TODO: Java initializer blocks (static/instance) detected; manual mapping needed.");
      }

      // Emit sections in ABAP order
      emitSection(out, Section.PUBLIC, fieldsBySec, methodsBySec);
      emitSection(out, Section.PROTECTED, fieldsBySec, methodsBySec);
      emitSection(out, Section.PRIVATE, fieldsBySec, methodsBySec);

      out.append("ENDCLASS.\n\n");

      // Implementation
      out.append("CLASS ").append(abapName).append(" IMPLEMENTATION.\n");

      // Constructors (only first)
      if (!ctors.isEmpty()) {
        ConstructorDeclaration cd = ctors.get(0);
        out.append("  METHOD constructor.\n");
        if (cd.getBody() != null) {
          for (Statement st : cd.getBody().getStatements()) {
            translateStatement(st, out, 2);
          }
        }
        out.append("  ENDMETHOD.\n\n");
      }

      // Methods bodies
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
      out.append("Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/interface/record/enum.\n");
      out.append(shortMessage(e)).append("\n");
    }

    return ensureTrailingNewline(out);
  }

  private enum Section { PUBLIC, PROTECTED, PRIVATE }

  private Section sectionOf(BodyDeclaration<?> d) {
    if (d == null) return Section.PUBLIC;
    if (d instanceof NodeWithPublic<?> pub && pub.isPublic()) return Section.PUBLIC;
    if (d instanceof NodeWithProtected<?> pro && pro.isProtected()) return Section.PROTECTED;
    if (d instanceof NodeWithPrivate<?> pri && pri.isPrivate()) return Section.PRIVATE;
    // package-private -> no ABAP equivalent
    return Section.PROTECTED;
  }

  private void emitSection(
      StringBuilder out,
      Section sec,
      Map<Section, List<FieldDeclaration>> fieldsBySec,
      Map<Section, List<CallableDeclaration<?>>> methodsBySec
  ) {
    String header = switch (sec) {
      case PUBLIC -> "  PUBLIC SECTION.";
      case PROTECTED -> "  PROTECTED SECTION.";
      case PRIVATE -> "  PRIVATE SECTION.";
    };
    out.append(header).append("\n");

    boolean any = false;

    // Fields
    for (FieldDeclaration fd : fieldsBySec.getOrDefault(sec, List.of())) {
      any = true;
      emitLeadingComments(fd, out, 4);

      String hint = fieldModifiersHint(fd);
      boolean packagePrivate = !(fd.isPublic() || fd.isProtected() || fd.isPrivate());
      if (packagePrivate) emit(out, 4, "\" Java visibility: package-private (no direct ABAP equivalent; mapped to " + sec + ")");

      if (!hint.isEmpty()) emit(out, 4, "\" " + hint);

      for (VariableDeclarator v : fd.getVariables()) {
        String name = v.getNameAsString();
        String type = mapType(v.getTypeAsString());

        boolean isConstCandidate = fd.isStatic() && fd.isFinal() && v.getInitializer().isPresent() && isSimpleLiteral(v.getInitializer().get());
        if (isConstCandidate) {
          emit(out, 4, "CONSTANTS " + name + " TYPE " + type + " VALUE " + expr(v.getInitializer().get()) + ".");
          continue;
        }

        String kw = fd.isStatic() ? "CLASS-DATA" : "DATA";
        String line = kw + " " + name + " TYPE " + type + ".";
        emit(out, 4, line);

        if (fd.isFinal() && !fd.isStatic()) {
          emit(out, 4, "\" TODO: final instance field (ABAP has no strict equivalent; consider READ-ONLY + no setter)");
        }

        v.getInitializer().ifPresent(init -> emit(out, 4, name + " = " + expr(init) + "."));
      }
    }

    // Enums inside class -> hint (kept)
    if (sec == Section.PUBLIC) {
      // only once to avoid noise
      // (rare in Java, but users asked for it)
    }

    // Methods / Constructor
    for (CallableDeclaration<?> cd : methodsBySec.getOrDefault(sec, List.of())) {
      any = true;

      if (cd instanceof MethodDeclaration md) {
        emitLeadingComments(md, out, 4);

        boolean packagePrivate = !(md.isPublic() || md.isProtected() || md.isPrivate());
        if (packagePrivate) emit(out, 4, "\" Java visibility: package-private (no direct ABAP equivalent; mapped to " + sec + ")");

        String hint = methodModifiersHint(md);
        if (!hint.isEmpty()) emit(out, 4, "\" " + hint);

        String methKw = md.isStatic() ? "CLASS-METHODS " : "METHODS ";
        out.append("    ").append(methKw).append(md.getNameAsString());

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
      } else if (cd instanceof ConstructorDeclaration ctor) {
        emitLeadingComments(ctor, out, 4);
        emit(out, 4, "\" Java constructor -> ABAP CONSTRUCTOR (no overload)");
        out.append("    METHODS constructor");
        if (!ctor.getParameters().isEmpty()) {
          out.append(" IMPORTING");
          ctor.getParameters().forEach(p ->
              out.append(" ").append(p.getNameAsString())
                  .append(" TYPE ").append(mapType(p.getTypeAsString()))
          );
        }
        out.append(".\n");
      }
    }

    if (!any) {
      emit(out, 4, "\" (no members)");
    }
  }

  private boolean isSimpleLiteral(Expression e) {
    if (e == null) return false;
    return e.isIntegerLiteralExpr() || e.isBooleanLiteralExpr() || e.isStringLiteralExpr()
        || e.isCharLiteralExpr() || e.isDoubleLiteralExpr() || e.isLongLiteralExpr();
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

    if (st.isLabeledStmt()) {
      LabeledStmt ls = st.asLabeledStmt();
      emit(out, indent, "\" TODO: labeled statement '" + ls.getLabel().asString() + "' (no direct ABAP label control-flow)");
      translateStatement(ls.getStatement(), out, indent);
      return;
    }

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

    if (st.isDoStmt()) {
      DoStmt ds = st.asDoStmt();
      emit(out, indent, "\" do/while mapped to DO + EXIT (best-effort)");
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
      emit(out, indent, "\" TODO: throw -> RAISE EXCEPTION TYPE <cx_...> (exception class unknown)");
      emit(out, indent, "\" thrown expr: " + expr(ts.getExpression()));
      return;
    }

    if (st.isAssertStmt()) {
      AssertStmt as = st.asAssertStmt();
      emit(out, indent, "ASSERT " + expr(as.getCheck()) + ".");
      as.getMessage().ifPresent(m -> emit(out, indent, "\" assert message: " + expr(m)));
      return;
    }

    if (st.isExplicitConstructorInvocationStmt()) {
      ExplicitConstructorInvocationStmt ec = st.asExplicitConstructorInvocationStmt();
      String tgt = ec.isThis() ? "me->constructor" : "super->constructor";
      String args = ec.getArguments().stream().map(this::expr).collect(Collectors.joining(", "));
      emit(out, indent, "\" constructor call (best-effort): " + (ec.isThis() ? "this(...)" : "super(...)"));
      emit(out, indent, "\" TODO: map args to ABAP constructor params manually: " + args);
      emit(out, indent, tgt + "( ).");
      return;
    }

    if (st.isBreakStmt()) {
      BreakStmt bs = st.asBreakStmt();
      if (bs.getLabel().isPresent()) {
        emit(out, indent, "\" TODO: break with label '" + bs.getLabel().get().asString() + "' (no direct ABAP equivalent)");
      }
      emit(out, indent, "EXIT.");
      return;
    }

    if (st.isContinueStmt()) {
      emit(out, indent, "CONTINUE.");
      return;
    }

    emit(out, indent, "\" TODO statement: " + prettyNodeName(st.getClass().getSimpleName()));
  }

  private void translateTryStmt(TryStmt ts, StringBuilder out, int indent) {
    emit(out, indent, "TRY.");
    for (Statement st : ts.getTryBlock().getStatements()) {
      translateStatement(st, out, indent + 2);
    }

    for (CatchClause cc : ts.getCatchClauses()) {
      String javaExType = cc.getParameter().getTypeAsString();
      String var = cc.getParameter().getNameAsString();
      emit(out, indent, "CATCH cx_root INTO DATA(" + var + ").");
      emit(out, indent + 2, "\" TODO: Java catch(" + javaExType + " " + var + ") -> ABAP exception class mapping needed");
      for (Statement st : cc.getBody().getStatements()) {
        translateStatement(st, out, indent + 2);
      }
    }

    if (ts.getFinallyBlock().isPresent()) {
      emit(out, indent, "FINALLY.");
      for (Statement st : ts.getFinallyBlock().get().getStatements()) {
        translateStatement(st, out, indent + 2);
      }
    }

    emit(out, indent, "ENDTRY.");
  }

  // ---------------------------
  // EXPRESSION STATEMENTS
  // ---------------------------
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
      String target = expr(ae.getTarget());
      String value = expr(ae.getValue());

      switch (ae.getOperator()) {
        case ASSIGN -> emit(out, indent, target + " = " + value + ".");
        case PLUS -> emit(out, indent, target + " = " + target + " + " + value + ".");
        case MINUS -> emit(out, indent, target + " = " + target + " - " + value + ".");
        case MULTIPLY -> emit(out, indent, target + " = " + target + " * " + value + ".");
        case DIVIDE -> emit(out, indent, target + " = " + target + " / " + value + ".");
        default -> {
          emit(out, indent, "\" TODO: compound assignment '" + ae.getOperator() + "' not mapped");
          emit(out, indent, target + " = " + value + ".");
        }
      }
      return;
    }

    if (e.isUnaryExpr()) {
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
        String arg = !mc.getArguments().isEmpty() ? expr(mc.getArgument(0)) : "''";
        emit(out, indent, "WRITE: / " + arg + ".");
        return;
      }
      if (isSystemOutPrint(mc)) {
        String arg = !mc.getArguments().isEmpty() ? expr(mc.getArgument(0)) : "''";
        emit(out, indent, "WRITE " + arg + ".");
        return;
      }

      // generic call: this/super/obj/ClassName
      String call = abapMethodCallStatement(mc);
      emit(out, indent, call);
      return;
    }

    if (e.isObjectCreationExpr()) {
      ObjectCreationExpr oce = e.asObjectCreationExpr();
      emit(out, indent, "\" TODO: object creation used as statement; usually assign to var");
      emit(out, indent, "\" new " + oce.getTypeAsString() + "(...) -> consider DATA ref + CREATE OBJECT");
      return;
    }

    emit(out, indent, "\" TODO expression: " + prettyNodeName(e.getClass().getSimpleName()));
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

  private String abapMethodCallStatement(MethodCallExpr mc) {
    String target;
    if (mc.getScope().isPresent()) {
      target = abapCallTarget(mc.getScope().get()) + mc.getNameAsString();
    } else {
      // unqualified call inside instance method -> me->
      target = "me->" + mc.getNameAsString();
    }

    if (mc.getArguments().isEmpty()) {
      return target + "( ).";
    }

    String args = mc.getArguments().stream().map(this::expr).collect(Collectors.joining(", "));
    return "\" TODO: map Java args to ABAP named parameters manually: (" + args + ")\n"
        + target + "( ).";
  }

  private String abapCallTarget(Expression scope) {
    // returns something like "me->" / "super->" / "lo_obj->" / "zcl_x=>"
    if (scope == null) return "me->";

    if (scope.isThisExpr()) return "me->";
    if (scope.isSuperExpr()) return "super->";

    if (scope.isNameExpr()) {
      String n = scope.asNameExpr().getNameAsString();
      if (looksLikeTypeName(n)) {
        return "zcl_" + toSnakeLower(n) + "=>";
      }
      return n + "->";
    }

    if (scope.isFieldAccessExpr()) {
      // chain like this.foo.bar -> me->foo->bar
      String base = access(scope);
      // access() already returns full, so to call method we need add "->"
      // but if base ends with "=>" keep static
      if (base.endsWith("=>")) return base;
      return base + "->";
    }

    // fallback
    return expr(scope) + "->";
  }

  // ---------------------------
  // SWITCH
  // ---------------------------
  private void translateSwitchStmt(SwitchStmt ss, StringBuilder out, int indent) {
    String selector = expr(ss.getSelector());
    emit(out, indent, "CASE " + selector + ".");

    List<SwitchEntry> entries = ss.getEntries();
    int i = 0;
    while (i < entries.size()) {
      SwitchEntry entry = entries.get(i);

      List<String> labels = new ArrayList<>();
      boolean isDefault = entry.getLabels().isEmpty();

      if (!isDefault) {
        labels.addAll(entry.getLabels().stream().map(this::switchLabelExpr).toList());
      }

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

      if (isDefault || labels.isEmpty()) {
        emit(out, indent, "  WHEN OTHERS.");
      } else {
        String when = String.join(" OR ", labels);
        emit(out, indent, "  WHEN " + when + ".");
      }

      for (Statement st : stmts) {
        if (st.isBreakStmt()) continue; // CASE in ABAP needs no break
        translateStatement(st, out, indent + 4);
      }

      i = j + 1;
    }

    emit(out, indent, "ENDCASE.");
  }

  private String switchLabelExpr(Expression e) {
    if (e == null) return "''";
    if (e.isNameExpr()) return "'" + e.asNameExpr().getNameAsString() + "'";
    if (e.isFieldAccessExpr()) return "'" + e.asFieldAccessExpr().getNameAsString() + "'";
    if (e.isStringLiteralExpr()) return "'" + e.asStringLiteralExpr().asString().replace("'", "''") + "'";
    if (e.isCharLiteralExpr()) return "'" + e.asCharLiteralExpr().asChar() + "'";
    if (e.isIntegerLiteralExpr()) return e.asIntegerLiteralExpr().getValue();
    if (e.isBooleanLiteralExpr()) return e.asBooleanLiteralExpr().getValue() ? "abap_true" : "abap_false";
    return "'" + prettyNodeName(e.getClass().getSimpleName()).replace(" ", "_").toUpperCase(Locale.ROOT) + "'";
  }

  // ---------------------------
  // FOR / FOREACH
  // ---------------------------
  private void translateForStmt(ForStmt fs, StringBuilder out, int indent) {
    Optional<ForLoopPattern> pat = detectSimpleCountingLoop(fs);
    if (pat.isPresent()) {
      ForLoopPattern p = pat.get();
      emit(out, indent, "\" for-loop mapped to DO...TIMES (best-effort)");
      emit(out, indent, "DO " + p.timesExpr + " TIMES.");
      emit(out, indent + 2, p.varName + " = sy-index - 1.");
      translateStatement(fs.getBody(), out, indent + 2);
      emit(out, indent, "ENDDO.");
      return;
    }

    emit(out, indent, "\" TODO statement: for-loop (non-trivial). Manual rewrite needed.");
    translateStatement(fs.getBody(), out, indent);
  }

  private void translateForEachStmt(ForEachStmt fes, StringBuilder out, int indent) {
    String var = fes.getVariable().getVariable(0).getNameAsString();
    String iterable = expr(fes.getIterable());

    emit(out, indent, "\" foreach mapped to LOOP AT (best-effort)");
    emit(out, indent, "LOOP AT " + iterable + " INTO " + var + ".");
    translateStatement(fes.getBody(), out, indent + 2);
    emit(out, indent, "ENDLOOP.");
  }

  private record ForLoopPattern(String varName, String timesExpr) {}

  private Optional<ForLoopPattern> detectSimpleCountingLoop(ForStmt fs) {
    if (fs.getInitialization().size() != 1) return Optional.empty();
    if (fs.getCompare().isEmpty()) return Optional.empty();
    if (fs.getUpdate().size() != 1) return Optional.empty();

    String varName;

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
      if (varName == null) return Optional.empty();
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
  // ENUM (basic hint)
  // ---------------------------
  private void translateEnumDeclaration(EnumDeclaration ed, StringBuilder out, int indent) {
    emitLeadingComments(ed, out, indent);
    String name = ed.getNameAsString();
    String values = ed.getEntries().stream()
        .map(EnumConstantDeclaration::getNameAsString)
        .collect(Collectors.joining(", "));
    emit(out, indent, "\" Enum detected: " + name + " = " + values);
    emit(out, indent, "\" Hint: ABAP has no direct Java-enum equivalent in syntax.");
    emit(out, indent, "\" Options: (1) DOMAIN + fixed values, (2) CONSTANTS in class/interface, (3) CHAR/STRING + validation.");
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
  // EXPR
  // ---------------------------
  private String expr(Expression e) {
    if (e == null) return "''";

    if (e.isThisExpr()) return "me";
    if (e.isSuperExpr()) return "super";

    if (e.isNameExpr()) return e.asNameExpr().getNameAsString();
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
      if (isSystemOutPrintln(mc) || isSystemOutPrint(mc)) return "''";
      // ABAP has no expression-style method call like Java; emit hint
      return "\" TODO: method-call expression -> ABAP needs CALL or inline DATA(...) for return";
    }

    if (e.isObjectCreationExpr()) {
      ObjectCreationExpr oce = e.asObjectCreationExpr();
      return "\" TODO: new " + oce.getTypeAsString() + "(...) -> ABAP CREATE OBJECT + ref var";
    }

    if (e.isConditionalExpr()) {
      return "COND #( WHEN " + expr(e.asConditionalExpr().getCondition()) +
          " THEN " + expr(e.asConditionalExpr().getThenExpr()) +
          " ELSE " + expr(e.asConditionalExpr().getElseExpr()) + " )";
    }

    if (e.isCastExpr()) {
      CastExpr ce = e.asCastExpr();
      return "\" TODO: cast (" + ce.getTypeAsString() + ") -> ABAP uses CAST/CONV; manual mapping";
    }

    if (e.isInstanceOfExpr()) {
      InstanceOfExpr io = e.asInstanceOfExpr();
      return "\" TODO: instanceof -> ABAP: RTTI (cl_abap_typedescr) / IS INSTANCE OF";
    }

    return "''";
  }

  private String access(FieldAccessExpr fa) {
    // Handle chains: this.a.b -> me->a->b; super.x -> super->x
    Expression scope = fa.getScope();

    if (scope.isThisExpr()) return "me->" + fa.getNameAsString();
    if (scope.isSuperExpr()) return "super->" + fa.getNameAsString();

    if (scope.isNameExpr()) {
      String s = scope.asNameExpr().getNameAsString();
      if (looksLikeTypeName(s)) {
        // static access: Foo.BAR -> zcl_foo=>bar (best-effort)
        return "zcl_" + toSnakeLower(s) + "=>" + toSnakeLower(fa.getNameAsString());
      }
      // obj.field -> obj->field
      return s + "->" + fa.getNameAsString();
    }

    if (scope.isFieldAccessExpr()) {
      String base = access(scope.asFieldAccessExpr());
      if (base.endsWith("=>")) return base + toSnakeLower(fa.getNameAsString());
      return base + "->" + fa.getNameAsString();
    }

    return expr(scope) + "->" + fa.getNameAsString();
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
  // HELPERS
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

  private boolean looksLikeTypeName(String n) {
    if (n == null || n.isBlank()) return false;
    // simple heuristic: Java class names often start with uppercase
    return Character.isUpperCase(n.charAt(0));
  }
}
