package de.example.j2abap;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.*;
import com.github.javaparser.ast.comments.BlockComment;
import com.github.javaparser.ast.comments.Comment;
import com.github.javaparser.ast.comments.JavadocComment;
import com.github.javaparser.ast.comments.LineComment;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.nodeTypes.NodeWithPrivate;
import com.github.javaparser.ast.nodeTypes.NodeWithProtected;
import com.github.javaparser.ast.nodeTypes.NodeWithPublic;
import com.github.javaparser.ast.stmt.*;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Best-effort Java -> ABAP translator for basic constructs.
 * Notes:
 * - ABAP is not Java: many constructs are mapped as hints/comments.
 * - Focus is "CV-demo translator": readable ABAP-ish output > perfect semantic equivalence.
 */
@Service
public class JavaToAbapTranslator {

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
      out.append("Parse error (Java). Tipp: Snippet braucht gültige Statements (meist mit ;).").append("\n");
      out.append(shortMessage(e)).append("\n");
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

      emitCompilationUnitHeaderComments(cu, out);

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

      // ---------------------------
      // INTERFACE
      // ---------------------------
      if (clsOpt.isPresent() && clsOpt.get().isInterface()) {
        ClassOrInterfaceDeclaration itf = clsOpt.get();
        String javaName = itf.getNameAsString();
        String abapName = "zif_" + toSnakeLower(javaName);

        emitLeadingComments(itf, out, 0);

        out.append("INTERFACE ").append(abapName).append(" PUBLIC.\n");

        // Fields in interfaces are implicitly public static final in Java
        for (FieldDeclaration fd : itf.getFields()) {
          emitLeadingComments(fd, out, 2);
          emit(out, 2, "\" Java interface field => constants (best-effort)");
          for (VariableDeclarator v : fd.getVariables()) {
            if (v.getInitializer().isPresent() && isSimpleLiteral(v.getInitializer().get())) {
              emit(out, 2, "CONSTANTS " + v.getNameAsString() + " TYPE " + mapType(v.getTypeAsString())
                  + " VALUE " + expr(v.getInitializer().get()) + ".");
            } else {
              emit(out, 2, "\" TODO: interface field without simple literal -> manual mapping");
            }
          }
        }

        // method signatures
        for (MethodDeclaration md : itf.getMethods()) {
          emitLeadingComments(md, out, 2);

          String hint = methodModifiersHint(md);
          if (!hint.isEmpty()) emit(out, 2, "\" " + hint);

          out.append("  METHODS ").append(md.getNameAsString());

          if (!md.getParameters().isEmpty()) {
            out.append(" IMPORTING");
            md.getParameters().forEach(p -> {
              AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), true);
              if (!ti.preLines.isEmpty()) {
                emit(out, 2, "\" TODO: complex param type in signature; manual TYPES needed for: " + p.getTypeAsString());
              }
              out.append(" ").append(p.getNameAsString())
                  .append(" TYPE ").append(ti.abapType);
            });
          }

          if (!md.getType().isVoidType()) {
            AbapTypeInfo rti = mapTypeInfo(md.getTypeAsString(), "rv_result", true);
            out.append(" RETURNING VALUE(rv_result) TYPE ").append(rti.abapType);
          }

          out.append(".\n");
        }

        emit(out, 2, "\" TODO: default methods / private methods in interfaces are not mapped 1:1");
        out.append("ENDINTERFACE.\n");

        return ensureTrailingNewline(out);
      }

      // ---------------------------
      // RECORD -> treat like class with readonly components + constructor hint
      // ---------------------------
      if (recOpt.isPresent()) {
        RecordDeclaration rec = recOpt.get();
        String javaName = rec.getNameAsString();
        String abapName = "zcl_" + toSnakeLower(javaName);

        emitLeadingComments(rec, out, 0);

        out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");
        out.append("  PUBLIC SECTION.\n");

        emit(out, 4, "\" record mapped best-effort: components -> READ-ONLY attributes");
        for (Parameter p : rec.getParameters()) {
          AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), false);
          for (String pre : ti.preLines) emit(out, 4, pre);
          for (String c : ti.comments) emit(out, 4, c);
          emit(out, 4, "DATA " + p.getNameAsString() + " TYPE " + ti.abapType + " READ-ONLY.");
        }

        out.append("    METHODS constructor");
        if (!rec.getParameters().isEmpty()) {
          out.append(" IMPORTING");
          for (Parameter p : rec.getParameters()) {
            AbapTypeInfo pti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), true);
            out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(pti.abapType);
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

      // ---------------------------
      // CLASS
      // ---------------------------
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

      // Enums inside class -> hint
      List<EnumDeclaration> nestedEnums = cls.findAll(EnumDeclaration.class).stream()
          .filter(ed -> ed.getParentNode().isPresent() && ed.getParentNode().get() instanceof ClassOrInterfaceDeclaration)
          .toList();
      if (!nestedEnums.isEmpty()) {
        emit(out, 0, "\" NOTE: Nested enums detected; ABAP has no direct nested-enum syntax. Consider constants/DOMAIN.");
        for (EnumDeclaration ed : nestedEnums) {
          emit(out, 0, "\"   Enum: " + ed.getNameAsString() + " = "
              + ed.getEntries().stream().map(EnumConstantDeclaration::getNameAsString).collect(Collectors.joining(", ")));
        }
      }

      // Emit sections in ABAP order
      emitSection(out, Section.PUBLIC, fieldsBySec, methodsBySec);
      emitSection(out, Section.PROTECTED, fieldsBySec, methodsBySec);
      emitSection(out, Section.PRIVATE, fieldsBySec, methodsBySec);

      out.append("ENDCLASS.\n\n");

      // ---------------------------
      // Implementation
      // ---------------------------
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
    // package-private -> no ABAP equivalent; choose PROTECTED (more restrictive than public)
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

      boolean packagePrivate = !(fd.isPublic() || fd.isProtected() || fd.isPrivate());
      if (packagePrivate) {
        emit(out, 4, "\" Java visibility: package-private (no direct ABAP equivalent; mapped to " + sec + ")");
      }

      String hint = fieldModifiersHint(fd);
      if (!hint.isEmpty()) emit(out, 4, "\" " + hint);

      for (VariableDeclarator v : fd.getVariables()) {
        emitDataDecl(out, 4, v.getNameAsString(), v.getTypeAsString(), v.getInitializer(), fd.isStatic(), fd.isFinal());
      }
    }

    // Methods / Constructor
    for (CallableDeclaration<?> cd : methodsBySec.getOrDefault(sec, List.of())) {
      any = true;

      if (cd instanceof MethodDeclaration md) {
        emitLeadingComments(md, out, 4);

        boolean packagePrivate = !(md.isPublic() || md.isProtected() || md.isPrivate());
        if (packagePrivate) {
          emit(out, 4, "\" Java visibility: package-private (no direct ABAP equivalent; mapped to " + sec + ")");
        }

        String hint = methodModifiersHint(md);
        if (!hint.isEmpty()) emit(out, 4, "\" " + hint);

        String methKw = md.isStatic() ? "CLASS-METHODS " : "METHODS ";
        out.append("    ").append(methKw).append(md.getNameAsString());

        if (!md.getParameters().isEmpty()) {
          out.append(" IMPORTING");
          md.getParameters().forEach(p -> {
            AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), true);
            if (!ti.preLines.isEmpty()) {
              emit(out, 4, "\" TODO: param type needs local TYPES; signature uses TYPE " + ti.abapType + " for: " + p.getTypeAsString());
            }
            out.append(" ").append(p.getNameAsString())
                .append(" TYPE ").append(ti.abapType);
          });
        }

        if (!md.getType().isVoidType()) {
          AbapTypeInfo rti = mapTypeInfo(md.getTypeAsString(), "rv_result", true);
          out.append(" RETURNING VALUE(rv_result) TYPE ").append(rti.abapType);
        }

        out.append(".\n");
      } else if (cd instanceof ConstructorDeclaration ctor) {
        emitLeadingComments(ctor, out, 4);
        emit(out, 4, "\" Java constructor -> ABAP CONSTRUCTOR (no overload)");
        out.append("    METHODS constructor");
        if (!ctor.getParameters().isEmpty()) {
          out.append(" IMPORTING");
          ctor.getParameters().forEach(p -> {
            AbapTypeInfo ti = mapTypeInfo(p.getTypeAsString(), p.getNameAsString(), true);
            out.append(" ").append(p.getNameAsString())
                .append(" TYPE ").append(ti.abapType);
          });
        }
        out.append(".\n");
      }
    }

    if (!any) {
      emit(out, 4, "\" (no members)");
    }
  }

  // ===========================
  // STATEMENTS
  // ===========================

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

  // ===========================
  // EXPRESSION STATEMENTS
  // ===========================

  private void translateExpressionStmt(ExpressionStmt es, StringBuilder out, int indent) {
    Expression e = es.getExpression();
    emitLeadingComments(e, out, indent);

    // ---------------------------
    // Variable declarations
    // ---------------------------
    if (e.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = e.asVariableDeclarationExpr();
      vde.getVariables().forEach(v -> {
        // DATA decl (with List/Map handling)
        emitDataDecl(out, indent, v.getNameAsString(), v.getTypeAsString(), Optional.empty(), false, false);

        // initializer special cases (get/new/null)
        if (v.getInitializer().isPresent()) {
          Expression init = v.getInitializer().get();

          // null -> CLEAR
          if (init.isNullLiteralExpr()) {
            emit(out, indent, "CLEAR " + v.getNameAsString() + ".");
            return;
          }

          // list.get(i) / map.get(k) used in initializer -> READ TABLE pattern
          if (init.isMethodCallExpr()) {
            MethodCallExpr mc = init.asMethodCallExpr();
            if (tryTranslateListGetIntoTarget(mc, v.getNameAsString(), out, indent).translated) return;
            if (tryTranslateMapGetIntoTarget(mc, v.getNameAsString(), out, indent).translated) return;
          }

          // new X(...) -> best-effort CREATE OBJECT if target looks like ref
          if (init.isObjectCreationExpr()) {
            ObjectCreationExpr oce = init.asObjectCreationExpr();
            if (looksLikeRefVar(v.getNameAsString())) {
              emit(out, indent, "\" new " + oce.getTypeAsString() + "(...) -> CREATE OBJECT (best-effort)");
              emit(out, indent, "CREATE OBJECT " + v.getNameAsString() + ".");
              if (!oce.getArguments().isEmpty()) {
                emit(out, indent, "\" TODO: constructor args need named parameters: "
                    + oce.getArguments().stream().map(this::expr).collect(Collectors.joining(", ")));
              }
            } else {
              emit(out, indent, "\" TODO: new " + oce.getTypeAsString() + "(...) -> ABAP needs ref var + CREATE OBJECT");
            }
            return;
          }

          // default
          emit(out, indent, v.getNameAsString() + " = " + expr(init) + ".");
        }
      });
      return;
    }

    // ---------------------------
    // Assignments
    // ---------------------------
    if (e.isAssignExpr()) {
      AssignExpr ae = e.asAssignExpr();
      String target = expr(ae.getTarget());

      // x = null -> CLEAR x.
      if (ae.getValue().isNullLiteralExpr() && ae.getOperator() == AssignExpr.Operator.ASSIGN) {
        emit(out, indent, "CLEAR " + target + ".");
        return;
      }

      // x = list.get(i) / map.get(k) -> READ TABLE ... INTO x.
      if (ae.getOperator() == AssignExpr.Operator.ASSIGN && ae.getValue().isMethodCallExpr()) {
        MethodCallExpr mc = ae.getValue().asMethodCallExpr();
        if (tryTranslateListGetIntoTarget(mc, target, out, indent).translated) return;
        if (tryTranslateMapGetIntoTarget(mc, target, out, indent).translated) return;
      }

      // x = new X(...) -> CREATE OBJECT x (best-effort)
      if (ae.getOperator() == AssignExpr.Operator.ASSIGN && ae.getValue().isObjectCreationExpr()) {
        ObjectCreationExpr oce = ae.getValue().asObjectCreationExpr();
        if (looksLikeRefVar(target)) {
          emit(out, indent, "\" new " + oce.getTypeAsString() + "(...) -> CREATE OBJECT (best-effort)");
          emit(out, indent, "CREATE OBJECT " + target + ".");
          if (!oce.getArguments().isEmpty()) {
            emit(out, indent, "\" TODO: constructor args need named parameters: "
                + oce.getArguments().stream().map(this::expr).collect(Collectors.joining(", ")));
          }
        } else {
          emit(out, indent, "\" TODO: object creation assigned to non-ref var; manual mapping needed");
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
          emit(out, indent, "\" TODO: compound assignment '" + ae.getOperator() + "' not mapped");
          emit(out, indent, target + " = " + value + ".");
        }
      }
      return;
    }

    // ---------------------------
    // Unary ++ / --
    // ---------------------------
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

    // ---------------------------
    // Method calls as statements
    // ---------------------------
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

      // List basics: list.add(x) -> APPEND x TO list.
      if (mc.getScope().isPresent() && mc.getNameAsString().equals("add") && mc.getArguments().size() == 1) {
        String list = expr(mc.getScope().get());
        String val = expr(mc.getArgument(0));
        emit(out, indent, "APPEND " + val + " TO " + list + ".");
        return;
      }

      // List basics: list.clear() -> CLEAR list.
      if (mc.getScope().isPresent() && mc.getNameAsString().equals("clear") && mc.getArguments().isEmpty()) {
        String list = expr(mc.getScope().get());
        emit(out, indent, "CLEAR " + list + ".");
        return;
      }

      // Map basics: map.put(k,v) -> INSERT VALUE ... INTO TABLE map. (needs ty_*_entry)
      if (mc.getScope().isPresent() && mc.getNameAsString().equals("put") && mc.getArguments().size() == 2) {
        String map = expr(mc.getScope().get());
        String k = expr(mc.getArgument(0));
        String v = expr(mc.getArgument(1));
        emit(out, indent, "\" Java Map.put(k,v) mapped best-effort (requires entry type ty_<map>_entry with fields key/value)");
        emit(out, indent, "INSERT VALUE #( key = " + k + " value = " + v + " ) INTO TABLE " + map + ".");
        return;
      }

      // Generic call (this/super/obj/ClassName)
      String call = abapMethodCallStatement(mc);
      emit(out, indent, call);
      return;
    }

    // new X(...) as standalone statement (rare)
    if (e.isObjectCreationExpr()) {
      ObjectCreationExpr oce = e.asObjectCreationExpr();
      emit(out, indent, "\" TODO: object creation used as statement; usually assign to var");
      emit(out, indent, "\" new " + oce.getTypeAsString() + "(...) -> consider DATA ref + CREATE OBJECT");
      return;
    }

    emit(out, indent, "\" TODO expression: " + prettyNodeName(e.getClass().getSimpleName()));
  }

  // ===========================
  // LIST.GET / MAP.GET (statement-level translation)
  // ===========================

  private record TranslationAttempt(boolean translated) {}

  private TranslationAttempt tryTranslateListGetIntoTarget(MethodCallExpr mc, String target, StringBuilder out, int indent) {
    if (mc == null) return new TranslationAttempt(false);
    if (!mc.getNameAsString().equals("get")) return new TranslationAttempt(false);
    if (mc.getScope().isEmpty()) return new TranslationAttempt(false);
    if (mc.getArguments().size() != 1) return new TranslationAttempt(false);

    String list = expr(mc.getScope().get());
    Expression idxExpr = mc.getArgument(0);

    emit(out, indent, "\" Java list.get(i) -> READ TABLE ... INDEX ... (0-based -> 1-based)");
    if (idxExpr.isIntegerLiteralExpr()) {
      int i0 = Integer.parseInt(idxExpr.asIntegerLiteralExpr().getValue());
      int i1 = i0 + 1;
      emit(out, indent, "READ TABLE " + list + " INDEX " + i1 + " INTO " + target + ".");
      emit(out, indent, "IF sy-subrc <> 0.");
      emit(out, indent + 2, "CLEAR " + target + ".");
      emit(out, indent, "ENDIF.");
      return new TranslationAttempt(true);
    }

    // dynamic index: create temp
    String idx = expr(idxExpr);
    emit(out, indent, "DATA(lv_idx) = " + idx + " + 1.");
    emit(out, indent, "READ TABLE " + list + " INDEX lv_idx INTO " + target + ".");
    emit(out, indent, "IF sy-subrc <> 0.");
    emit(out, indent + 2, "CLEAR " + target + ".");
    emit(out, indent, "ENDIF.");
    return new TranslationAttempt(true);
  }

  private TranslationAttempt tryTranslateMapGetIntoTarget(MethodCallExpr mc, String target, StringBuilder out, int indent) {
    if (mc == null) return new TranslationAttempt(false);
    if (!mc.getNameAsString().equals("get")) return new TranslationAttempt(false);
    if (mc.getScope().isEmpty()) return new TranslationAttempt(false);
    if (mc.getArguments().size() != 1) return new TranslationAttempt(false);

    String map = expr(mc.getScope().get());
    String key = expr(mc.getArgument(0));

    emit(out, indent, "\" Java map.get(k) -> READ TABLE ... WITH TABLE KEY key = k (best-effort)");
    emit(out, indent, "\" NOTE: requires map to be a hashed table of entries with fields key/value");
    emit(out, indent, "READ TABLE " + map + " WITH TABLE KEY key = " + key + " INTO DATA(ls_entry).");
    emit(out, indent, "IF sy-subrc = 0.");
    emit(out, indent + 2, target + " = ls_entry-value.");
    emit(out, indent, "ELSE.");
    emit(out, indent + 2, "CLEAR " + target + ".");
    emit(out, indent, "ENDIF.");
    return new TranslationAttempt(true);
  }

  // ===========================
  // TRY/CATCH
  // ===========================

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
        if (st.isBreakStmt()) continue;
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

  // ===========================
  // FOR / FOREACH
  // ===========================

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
    // for (T x : list) { ... } => LOOP AT list INTO x. ... ENDLOOP.
    // If element type is "var" => LOOP AT ... INTO DATA(x).
    String var = fes.getVariable().getVariable(0).getNameAsString();
    String javaElemType = fes.getVariable().getElementType().asString();
    String iterable = expr(fes.getIterable());

    emit(out, indent, "\" foreach mapped to LOOP AT (best-effort)");

    boolean isVar = "var".equals(javaElemType);
    if (isVar) {
      emit(out, indent, "LOOP AT " + iterable + " INTO DATA(" + var + ").");
    } else {
      AbapTypeInfo ti = mapTypeInfo(javaElemType, var, false);
      for (String pre : ti.preLines) emit(out, indent, pre);
      for (String c : ti.comments) emit(out, indent, c);
      emit(out, indent, "\" NOTE: ensure '" + var + "' is declared with compatible type");
      emit(out, indent, "LOOP AT " + iterable + " INTO " + var + ".");
    }

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

  // ===========================
  // ENUM (basic hint)
  // ===========================

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

  // ===========================
  // COLLECTION / NULL HELPERS
  // ===========================

  private record AbapTypeInfo(String abapType, List<String> preLines, List<String> comments) {}

  private record Generic(String base, List<String> args) {}

  private AbapTypeInfo mapTypeInfo(String javaTypeRaw, String varName, boolean forSignature) {
    String t = javaTypeRaw == null ? "" : javaTypeRaw.trim();

    // Arrays: T[] -> internal table
    if (t.endsWith("[]")) {
      String elemJava = t.substring(0, t.length() - 2).trim();
      String elemAbap = mapType(elemJava);
      String abap = forSignature
          ? ("STANDARD TABLE OF " + elemAbap)
          : ("STANDARD TABLE OF " + elemAbap + " WITH EMPTY KEY");
      return new AbapTypeInfo(abap, List.of(), List.of("\" Java array -> ABAP internal table (STANDARD TABLE)"));
    }

    // Generics: List<T>, Map<K,V> ...
    Optional<Generic> g = parseGeneric(t);
    if (g.isPresent()) {
      Generic gg = g.get();

      if (isListBase(gg.base)) {
        String elemJava = gg.args.isEmpty() ? "String" : gg.args.get(0);
        String elemAbap = mapType(elemJava);

        if ("String".equals(elemJava) || "string".equalsIgnoreCase(elemAbap)) {
          return new AbapTypeInfo("string_table", List.of(),
              List.of("\" Java List<String> -> ABAP string_table"));
        }

        String abap = forSignature
            ? ("STANDARD TABLE OF " + elemAbap)
            : ("STANDARD TABLE OF " + elemAbap + " WITH EMPTY KEY");
        return new AbapTypeInfo(abap, List.of(),
            List.of("\" Java List<" + elemJava + "> -> ABAP internal table (STANDARD TABLE)"));
      }

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

        String abapType = forSignature
            ? "any"
            : ("HASHED TABLE OF " + ty + " WITH UNIQUE KEY key");

        List<String> cmts = List.of(
            "\" Java Map<" + keyJava + "," + valJava + "> -> ABAP hashed table needs entry type",
            "\" NOTE: In method signatures we fallback to TYPE any (manual refinement recommended)"
        );

        return new AbapTypeInfo(abapType, pre, cmts);
      }
    }

    return new AbapTypeInfo(mapType(t), List.of(), List.of());
  }

  private Optional<Generic> parseGeneric(String type) {
    if (type == null) return Optional.empty();
    String s = type.trim();
    int lt = s.indexOf('<');
    int gt = s.lastIndexOf('>');
    if (lt < 0 || gt < 0 || gt <= lt) return Optional.empty();

    String base = s.substring(0, lt).trim();
    String inside = s.substring(lt + 1, gt).trim();
    List<String> args = splitTopLevelArgs(inside);
    return Optional.of(new Generic(base, args));
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

  private boolean looksLikeRefVar(String abapOperand) {
    if (abapOperand == null) return false;
    String x = abapOperand.trim().toLowerCase(Locale.ROOT);
    return x.startsWith("lo_") || x.startsWith("lr_") || x.startsWith("ro_") || x.startsWith("rr_")
        || x.contains("->");
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

    for (String line : ti.preLines) emit(out, indent, line);
    for (String c : ti.comments) emit(out, indent, c);

    if (isStatic && isFinal && initializer.isPresent() && isSimpleLiteral(initializer.get())) {
      emit(out, indent, "CONSTANTS " + name + " TYPE " + mapType(javaType) + " VALUE " + expr(initializer.get()) + ".");
      return;
    }

    String kw = isStatic ? "CLASS-DATA" : "DATA";
    emit(out, indent, kw + " " + name + " TYPE " + ti.abapType + ".");

    if (initializer.isPresent()) {
      Expression init = initializer.get();
      if (init.isNullLiteralExpr()) {
        emit(out, indent, "CLEAR " + name + ".");
      } else if (init.isObjectCreationExpr()) {
        emit(out, indent, "\" TODO: initializer 'new ...' -> ABAP table is initial/empty by default; for objects use CREATE OBJECT");
      } else {
        emit(out, indent, name + " = " + expr(init) + ".");
      }
    }

    if (isFinal && !isStatic) {
      emit(out, indent, "\" TODO: final instance field (ABAP has no strict equivalent; consider READ-ONLY + no setter)");
    }
  }

  private boolean isSimpleLiteral(Expression e) {
    if (e == null) return false;
    return e.isIntegerLiteralExpr() || e.isBooleanLiteralExpr() || e.isStringLiteralExpr()
        || e.isCharLiteralExpr() || e.isDoubleLiteralExpr() || e.isLongLiteralExpr();
  }

  // ===========================
  // COMMENTS (Java -> ABAP)
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
      String[] lines = raw.split("\n");
      for (String line : lines) {
        String t = line.strip();
        if (!t.isEmpty()) emit(out, indent, "\" " + t);
      }
    }
  }

  // ===========================
  // MODIFIER HINTS
  // ===========================

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

  // ===========================
  // METHOD CALL STATEMENT EMISSION
  // ===========================

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
      String base = access(scope.asFieldAccessExpr());
      if (base.endsWith("=>")) return base;
      return base + "->";
    }

    return expr(scope) + "->";
  }

  // ===========================
  // EXPR (Java expression -> ABAP-ish expression)
  // ===========================

  private String expr(Expression e) {
    if (e == null) return "''";

    if (e.isNullLiteralExpr()) return "INITIAL";

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

      // null comparisons: x == null / x != null
      if ((b.getOperator() == BinaryExpr.Operator.EQUALS || b.getOperator() == BinaryExpr.Operator.NOT_EQUALS) &&
          (b.getLeft().isNullLiteralExpr() || b.getRight().isNullLiteralExpr())) {
        Expression other = b.getLeft().isNullLiteralExpr() ? b.getRight() : b.getLeft();
        String opnd = expr(other);

        boolean notEq = b.getOperator() == BinaryExpr.Operator.NOT_EQUALS;
        boolean ref = looksLikeRefVar(opnd);

        if (ref) {
          return notEq ? (opnd + " IS BOUND") : (opnd + " IS NOT BOUND");
        }
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
      if (u.getOperator() == UnaryExpr.Operator.LOGICAL_COMPLEMENT) {
        return "NOT (" + expr(u.getExpression()) + ")";
      }
      return expr(u.getExpression());
    }

    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();
      if (isSystemOutPrintln(mc) || isSystemOutPrint(mc)) return "''";

      // Objects.isNull(x) / Objects.nonNull(x)
      if (mc.getScope().isPresent() && mc.getScope().get().isNameExpr()
          && mc.getScope().get().asNameExpr().getNameAsString().equals("Objects")
          && mc.getArguments().size() == 1) {
        String x = expr(mc.getArgument(0));
        if (mc.getNameAsString().equals("isNull")) {
          return looksLikeRefVar(x) ? (x + " IS NOT BOUND") : (x + " IS INITIAL");
        }
        if (mc.getNameAsString().equals("nonNull")) {
          return looksLikeRefVar(x) ? (x + " IS BOUND") : (x + " IS NOT INITIAL");
        }
      }

      // list.size() -> lines( list )
      if (mc.getScope().isPresent() && mc.getArguments().isEmpty() && mc.getNameAsString().equals("size")) {
        return "lines( " + expr(mc.getScope().get()) + " )";
      }

      // list.isEmpty() -> lines( list ) = 0
      if (mc.getScope().isPresent() && mc.getArguments().isEmpty() && mc.getNameAsString().equals("isEmpty")) {
        return "lines( " + expr(mc.getScope().get()) + " ) = 0";
      }

      // list.get(i) / map.get(k) used inside expressions:
      // ABAP would need READ TABLE; we can't inline easily => comment placeholder
      if (mc.getScope().isPresent() && mc.getNameAsString().equals("get") && mc.getArguments().size() == 1) {
        return "\" TODO: get(...) in expression -> use READ TABLE ... INTO ... (cannot inline easily)";
      }

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
      return "\" TODO: instanceof -> ABAP: RTTI / IS INSTANCE OF (manual mapping)";
    }

    return "''";
  }

  private String access(FieldAccessExpr fa) {
    // Chains: this.a.b -> me->a->b ; super.x -> super->x ; Foo.BAR -> zcl_foo=>bar (best-effort)
    Expression scope = fa.getScope();

    if (scope.isThisExpr()) return "me->" + fa.getNameAsString();
    if (scope.isSuperExpr()) return "super->" + fa.getNameAsString();

    if (scope.isNameExpr()) {
      String s = scope.asNameExpr().getNameAsString();
      if (looksLikeTypeName(s)) {
        return "zcl_" + toSnakeLower(s) + "=>" + toSnakeLower(fa.getNameAsString());
      }
      return s + "->" + fa.getNameAsString();
    }

    if (scope.isFieldAccessExpr()) {
      String base = access(scope.asFieldAccessExpr());
      if (base.endsWith("=>")) return base + toSnakeLower(fa.getNameAsString());
      return base + "->" + fa.getNameAsString();
    }

    return expr(scope) + "->" + fa.getNameAsString();
  }

  // ===========================
  // System.out prints
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
  // Helpers
  // ===========================

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
    return Character.isUpperCase(n.charAt(0));
  }
}
