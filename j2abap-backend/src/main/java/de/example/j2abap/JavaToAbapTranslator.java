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

  // --- per-translation state ---
  private final Map<String, List<String>> enumConstantsByType = new LinkedHashMap<>();
  private final Map<String, List<String>> enumTypesByConstant = new HashMap<>();

  private final Map<String, String> mapVarToEntryType = new HashMap<>();
  private final Map<String, String> listVarToElemType = new HashMap<>();
  private final Set<String> declaredTemps = new HashSet<>();

  // ============================
  // Public API
  // ============================
  public String translateAuto(String javaCode) {
    String src = javaCode == null ? "" : javaCode.trim();
    if (src.isBlank()) return "\n";

    // Auto: if it's a valid CompilationUnit containing a top-level Type => class mode.
    // Otherwise snippet mode (also supports "mixed": enum + statements).
    try {
      CompilationUnit cu = StaticJavaParser.parse(src);
      boolean hasType = !cu.getTypes().isEmpty();
      return hasType ? translateClass(src) : translateSnippet(src);
    } catch (Exception ignore) {
      return translateSnippet(src);
    }
  }

  public String translateSnippet(String javaCode) {
    return translateSnippetInternal(javaCode == null ? "" : javaCode);
  }

  public String translateClass(String javaCode) {
    return translateClassInternal(javaCode == null ? "" : javaCode);
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

  // ============================
  // SNIPPET
  // - NO ABAP classes/reports created
  // - Supports unlimited statements
  // - Supports mixed inputs (enum/class/interface/record + statements)
  // ============================
  private String translateSnippetInternal(String src) {
    resetState();
    StringBuilder out = new StringBuilder();

    // Extract type declarations anywhere, put them into a wrapper class,
    // put remaining text into wrapper method body.
    Extracted ex = extractTopLevelTypeDeclsAnywhere(src == null ? "" : src);

    String wrapper =
        "class __J2ABAP {\n" +
            ex.extractedTypes +
            "  void __m() {\n" +
            ex.remainingCode + "\n" +
            "  }\n" +
            "}\n";

    try {
      CompilationUnit cu = StaticJavaParser.parse(wrapper);

      // Enums -> ABAP TYPES/CONSTANTS at snippet level
      indexEnums(cu);
      inferEnumsFromSwitches(cu);
      emitEnumAbapDeclarationsSnippet(out);

      // translate wrapper method statements
      MethodDeclaration m = cu.findFirst(MethodDeclaration.class, md -> md.getNameAsString().equals("__m"))
          .orElse(null);

      if (m == null || m.getBody().isEmpty()) {
        emit(out, 0, "\" TODO: no translatable statements found");
        return ensureTrailingNewline(out);
      }

      for (Statement st : m.getBody().get().getStatements()) {
        translateStatement(st, out, 0, Context.SNIPPET);
      }

    } catch (Exception e) {
      out.append("Parse error (Java). Tipp: Snippet braucht gültige Statements.\n");
      out.append(shortMessage(e)).append("\n");
    }

    return ensureTrailingNewline(out);
  }

  // ============================
  // CLASS
  // - Produces CLASS ... DEFINITION/IMPLEMENTATION ... ENDCLASS
  // - Translates method bodies
  // ============================
  private String translateClassInternal(String src) {
    resetState();
    StringBuilder out = new StringBuilder();

    try {
      CompilationUnit cu = StaticJavaParser.parse(src == null ? "" : src);

      emitCompilationUnitHeaderComments(cu, out);

      // Enums (also used for constant mapping in expressions)
      indexEnums(cu);
      inferEnumsFromSwitches(cu);
      
      // First top-level class/interface/record/enum
      Optional<ClassOrInterfaceDeclaration> clsOpt = cu.findFirst(ClassOrInterfaceDeclaration.class);
      Optional<RecordDeclaration> recOpt = cu.findFirst(RecordDeclaration.class);
      Optional<EnumDeclaration> enumOpt = cu.findFirst(EnumDeclaration.class);

      if (clsOpt.isEmpty() && recOpt.isEmpty()) {
        if (enumOpt.isPresent()) {
          // top-level enum file -> still emit something usable (snippet-like)
          emitEnumAbapDeclarationsSnippet(out);
          return ensureTrailingNewline(out);
        }
        return "Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/interface/record/enum.\n";
      }

      if (recOpt.isPresent() && clsOpt.isEmpty()) {
        // Keep record minimal but real class output
        RecordDeclaration rec = recOpt.get();
        String abapName = "zcl_" + toSnakeLower(rec.getNameAsString());

        emitLeadingComments(rec, out, 0);

        out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");
        out.append("  PUBLIC SECTION.\n");

        // record components -> DATA READ-ONLY
        for (Parameter p : rec.getParameters()) {
          emit(out, 4, "DATA " + p.getNameAsString() + " TYPE " + mapTypeForAbap(p.getTypeAsString()) + " READ-ONLY.");
        }

        // ABAP constructor
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

      ClassOrInterfaceDeclaration cls = clsOpt.get();
      String javaName = cls.getNameAsString();
      String abapName = (cls.isInterface() ? "zif_" : "zcl_") + toSnakeLower(javaName);

      emitLeadingComments(cls, out, 0);

      // INTERFACE
      if (cls.isInterface()) {
        out.append("INTERFACE ").append(abapName).append(" PUBLIC.\n");
        for (MethodDeclaration md : cls.getMethods()) {
          emitLeadingComments(md, out, 2);
          out.append("  METHODS ").append(md.getNameAsString());
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
        out.append("ENDINTERFACE.\n");
        return ensureTrailingNewline(out);
      }

      // CLASS DEFINITION
      out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");

      // group members into sections
      Map<Section, List<FieldDeclaration>> fields = new EnumMap<>(Section.class);
      Map<Section, List<MethodDeclaration>> methods = new EnumMap<>(Section.class);
      List<ConstructorDeclaration> ctors = cls.getConstructors();

      for (Section s : Section.values()) {
        fields.put(s, new ArrayList<>());
        methods.put(s, new ArrayList<>());
      }

      for (FieldDeclaration fd : cls.getFields()) fields.get(sectionOf(fd)).add(fd);
      for (MethodDeclaration md : cls.getMethods()) methods.get(sectionOf(md)).add(md);

      // If enums exist in the file, emit in PUBLIC SECTION (usable from everywhere)
      boolean hasEnums = !enumConstantsByType.isEmpty();

      // Emit sections
      emitClassSection(out, Section.PUBLIC, fields, methods, ctors, hasEnums, true);
      emitClassSection(out, Section.PROTECTED, fields, methods, ctors, hasEnums, false);
      emitClassSection(out, Section.PRIVATE, fields, methods, ctors, hasEnums, false);

      out.append("ENDCLASS.\n\n");

      // CLASS IMPLEMENTATION
      out.append("CLASS ").append(abapName).append(" IMPLEMENTATION.\n");

      // Constructors: ABAP has one CONSTRUCTOR; if multiple, translate first and mark others.
      if (!ctors.isEmpty()) {
        ConstructorDeclaration cd = ctors.get(0);
        out.append("  METHOD constructor.\n");
        if (cd.getBody() != null) {
          for (Statement st : cd.getBody().getStatements()) {
            translateStatement(st, out, 2, Context.CLASS);
          }
        }
        out.append("  ENDMETHOD.\n\n");

        if (ctors.size() > 1) {
          emit(out, 2, "\" TODO: multiple Java constructors exist; ABAP has no overload (only first translated).");
          out.append("\n");
        }
      }

      for (MethodDeclaration md : cls.getMethods()) {
        out.append("  METHOD ").append(md.getNameAsString()).append(".\n");

        if (md.getBody().isPresent()) {
          BlockStmt body = md.getBody().get();
          emitLeadingComments(body, out, 2);
          for (Statement st : body.getStatements()) {
            translateStatement(st, out, 2, Context.CLASS);
          }
        } else {
          emit(out, 2, "\" TODO: method body missing");
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

  // ============================
  // Section handling (no NodeWith* / no getModifiers())
  // ============================
  private enum Section { PUBLIC, PROTECTED, PRIVATE }
  private enum Context { SNIPPET, CLASS }

  private Section sectionOf(FieldDeclaration fd) {
    if (fd == null) return Section.PUBLIC;
    if (fd.isPublic()) return Section.PUBLIC;
    if (fd.isProtected()) return Section.PROTECTED;
    if (fd.isPrivate()) return Section.PRIVATE;
    return Section.PROTECTED; // package-private best-effort
  }

  private Section sectionOf(MethodDeclaration md) {
    if (md == null) return Section.PUBLIC;
    if (md.isPublic()) return Section.PUBLIC;
    if (md.isProtected()) return Section.PROTECTED;
    if (md.isPrivate()) return Section.PRIVATE;
    return Section.PROTECTED; // package-private best-effort
  }

  private Section sectionOf(ConstructorDeclaration cd) {
    if (cd == null) return Section.PUBLIC;
    if (cd.isPublic()) return Section.PUBLIC;
    if (cd.isProtected()) return Section.PROTECTED;
    if (cd.isPrivate()) return Section.PRIVATE;
    return Section.PROTECTED;
  }

  private void emitClassSection(
      StringBuilder out,
      Section sec,
      Map<Section, List<FieldDeclaration>> fields,
      Map<Section, List<MethodDeclaration>> methods,
      List<ConstructorDeclaration> ctors,
      boolean hasEnums,
      boolean allowEnumsHere
  ) {
    out.append(switch (sec) {
      case PUBLIC -> "  PUBLIC SECTION.\n";
      case PROTECTED -> "  PROTECTED SECTION.\n";
      case PRIVATE -> "  PRIVATE SECTION.\n";
    });

    // enums in PUBLIC SECTION
    if (sec == Section.PUBLIC && hasEnums && allowEnumsHere) {
      emit(out, 4, "\" --- enums (best-effort) ---");
      for (Map.Entry<String, List<String>> e : enumConstantsByType.entrySet()) {
        String enumType = e.getKey();
        String ty = "ty_" + toSnakeLower(enumType);
        emit(out, 4, "TYPES " + ty + " TYPE string.");
        for (String c : e.getValue()) {
          emit(out, 4, "CONSTANTS c_" + toSnakeLower(enumType) + "_" + toSnakeLower(c) +
              " TYPE " + ty + " VALUE '" + c + "'.");
        }
      }
      out.append("\n");
    }

    // fields
    for (FieldDeclaration fd : fields.getOrDefault(sec, List.of())) {
      emitLeadingComments(fd, out, 4);

      for (VariableDeclarator v : fd.getVariables()) {
        String name = v.getNameAsString();
        String javaType = v.getTypeAsString();

        // constants
        boolean constCandidate = fd.isStatic() && fd.isFinal()
            && v.getInitializer().isPresent()
            && isSimpleLiteral(v.getInitializer().get());

        if (constCandidate) {
          emit(out, 4, "CONSTANTS " + name + " TYPE " + mapTypeForAbap(javaType) + " VALUE " + expr(v.getInitializer().get(), Context.CLASS) + ".");
          continue;
        }

        // list/map -> proper ABAP table decls
        if (looksLikeListType(javaType)) {
          emitAbapListDecl(name, javaType, out, 4);
          continue;
        }
        if (looksLikeMapType(javaType)) {
          emitAbapMapDecl(name, javaType, out, 4);
          continue;
        }

        String kw = fd.isStatic() ? "CLASS-DATA" : "DATA";
        emit(out, 4, kw + " " + name + " TYPE " + mapTypeForAbap(javaType) + ".");
      }
    }

    // constructor signature (only first)
    if (!ctors.isEmpty() && sectionOf(ctors.get(0)) == sec) {
      ConstructorDeclaration cd = ctors.get(0);
      emitLeadingComments(cd, out, 4);

      out.append("    METHODS constructor");
      if (!cd.getParameters().isEmpty()) {
        out.append(" IMPORTING");
        for (Parameter p : cd.getParameters()) {
          out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(mapTypeForAbap(p.getTypeAsString()));
        }
      }
      out.append(".\n");
    }

    // methods
    for (MethodDeclaration md : methods.getOrDefault(sec, List.of())) {
      emitLeadingComments(md, out, 4);

      String kw = md.isStatic() ? "CLASS-METHODS " : "METHODS ";
      out.append("    ").append(kw).append(md.getNameAsString());

      if (!md.getParameters().isEmpty()) {
        out.append(" IMPORTING");
        for (Parameter p : md.getParameters()) {
          String pType = p.getTypeAsString();
          out.append(" ").append(p.getNameAsString()).append(" TYPE ").append(mapTypeForAbap(pType));
        }
      }

      if (!md.getType().isVoidType()) {
        out.append(" RETURNING VALUE(rv_result) TYPE ").append(mapTypeForAbap(md.getTypeAsString()));
      }

      out.append(".\n");
    }
  }

  // ============================
  // STATEMENTS
  // ============================
  private void translateStatement(Statement st, StringBuilder out, int indent, Context ctx) {
    if (st == null) return;

    emitLeadingComments(st, out, indent);

    if (st.isBlockStmt()) {
      for (Statement inner : st.asBlockStmt().getStatements()) {
        translateStatement(inner, out, indent, ctx);
      }
      return;
    }

    if (st.isEmptyStmt()) return;

    if (st.isExpressionStmt()) {
      translateExpressionStmt(st.asExpressionStmt(), out, indent, ctx);
      return;
    }

    if (st.isIfStmt()) {
      IfStmt is = st.asIfStmt();
      emit(out, indent, "IF " + expr(is.getCondition(), ctx) + ".");
      translateStatement(is.getThenStmt(), out, indent + 2, ctx);
      if (is.getElseStmt().isPresent()) {
        emit(out, indent, "ELSE.");
        translateStatement(is.getElseStmt().get(), out, indent + 2, ctx);
      }
      emit(out, indent, "ENDIF.");
      return;
    }

    if (st.isWhileStmt()) {
      WhileStmt ws = st.asWhileStmt();
      emit(out, indent, "WHILE " + expr(ws.getCondition(), ctx) + ".");
      translateStatement(ws.getBody(), out, indent + 2, ctx);
      emit(out, indent, "ENDWHILE.");
      return;
    }

    if (st.isDoStmt()) {
      DoStmt ds = st.asDoStmt();
      emit(out, indent, "DO.");
      translateStatement(ds.getBody(), out, indent + 2, ctx);
      emit(out, indent + 2, "IF NOT ( " + expr(ds.getCondition(), ctx) + " ).");
      emit(out, indent + 4, "EXIT.");
      emit(out, indent + 2, "ENDIF.");
      emit(out, indent, "ENDDO.");
      return;
    }

    if (st.isForEachStmt()) {
      translateForEachStmt(st.asForEachStmt(), out, indent, ctx);
      return;
    }

    if (st.isForStmt()) {
      translateForStmt(st.asForStmt(), out, indent, ctx);
      return;
    }

    if (st.isSwitchStmt()) {
      translateSwitchStmt(st.asSwitchStmt(), out, indent, ctx);
      return;
    }

    if (st.isTryStmt()) {
      translateTryStmt(st.asTryStmt(), out, indent, ctx);
      return;
    }

    if (st.isThrowStmt()) {
      ThrowStmt ts = st.asThrowStmt();
      emit(out, indent, "\" TODO: throw not translated (ABAP exception class unknown): " + ts.getExpression());
      return;
    }

    if (st.isReturnStmt()) {
      ReturnStmt rs = st.asReturnStmt();
      if (rs.getExpression().isPresent()) {
        emit(out, indent, "rv_result = " + expr(rs.getExpression().get(), ctx) + ".");
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

    if (st.isExplicitConstructorInvocationStmt()) {
      ExplicitConstructorInvocationStmt ec = st.asExplicitConstructorInvocationStmt();
      emit(out, indent, "\" TODO: " + (ec.isThis() ? "this(...)" : "super(...)") + " not translated (ABAP constructor mapping needs param names)");
      return;
    }

    emit(out, indent, "\" TODO: statement not mapped: " + prettyNodeName(st.getClass().getSimpleName()));
  }

  // ============================
  // EXPRESSION STATEMENTS
  // ============================
  private void translateExpressionStmt(ExpressionStmt es, StringBuilder out, int indent, Context ctx) {
    Expression e = es.getExpression();
    emitLeadingComments(e, out, indent);

    // variable declarations
    if (e.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = e.asVariableDeclarationExpr();

      for (VariableDeclarator v : vde.getVariables()) {
        String name = v.getNameAsString();
        String declaredType = v.getTypeAsString();

        // infer for "var"
        String effectiveType = declaredType;
        if ("var".equals(stripGeneric(declaredType)) && v.getInitializer().isPresent()) {
          Expression init = v.getInitializer().get();
          if (init.isObjectCreationExpr()) {
            effectiveType = init.asObjectCreationExpr().getTypeAsString();
          } else if (init.isStringLiteralExpr()) {
            effectiveType = "String";
          } else if (init.isBooleanLiteralExpr()) {
            effectiveType = "boolean";
          } else if (init.isIntegerLiteralExpr()) {
            effectiveType = "int";
          }
        }

        // List<T>
        if (looksLikeListType(effectiveType)) {
          emitAbapListDecl(name, effectiveType, out, indent);
          v.getInitializer().ifPresent(init -> {
            if (init.isObjectCreationExpr()) emit(out, indent, "CLEAR " + name + ".");
          });
          continue;
        }

        // Map<K,V>
        if (looksLikeMapType(effectiveType)) {
          emitAbapMapDecl(name, effectiveType, out, indent);
          v.getInitializer().ifPresent(init -> {
            if (init.isObjectCreationExpr()) emit(out, indent, "CLEAR " + name + ".");
          });
          continue;
        }

        // new -> REF + NEW
        if (v.getInitializer().isPresent() && v.getInitializer().get().isObjectCreationExpr()) {
          emitAbapNewVar(name, effectiveType, v.getInitializer().get().asObjectCreationExpr(), out, indent, ctx);
          continue;
        }

        // unknown object type -> REF
        if (looksLikeTypeName(stripGeneric(effectiveType)) && !isKnownValueType(stripGeneric(effectiveType))) {
          emit(out, indent, "DATA " + name + " TYPE REF TO " + mapJavaTypeToAbapClass(stripGeneric(effectiveType)) + ".");
        } else {
          emit(out, indent, "DATA " + name + " TYPE " + mapTypeForAbap(effectiveType) + ".");
        }

        if (v.getInitializer().isPresent()) {
          Expression init = v.getInitializer().get();

          // list.get / map.get in initializer -> READ TABLE
          if (init.isMethodCallExpr()) {
            MethodCallExpr mc = init.asMethodCallExpr();
            if (tryEmitListGetIntoTarget(mc, name, out, indent, ctx)) continue;
            if (tryEmitMapGetIntoTarget(mc, name, out, indent, ctx)) continue;
          }

          emit(out, indent, name + " = " + expr(init, ctx) + ".");
        }
      }
      return;
    }

    // assignment
    if (e.isAssignExpr()) {
      AssignExpr ae = e.asAssignExpr();
      String target = expr(ae.getTarget(), ctx);
      Expression val = ae.getValue();

      if (ae.getOperator() == AssignExpr.Operator.ASSIGN) {
        // a = new Auto();
        if (val.isObjectCreationExpr()) {
          emitAbapNewAssign(target, val.asObjectCreationExpr(), out, indent, ctx);
          return;
        }
        // v = list.get(0) / m.get("k")
        if (val.isMethodCallExpr()) {
          MethodCallExpr mc = val.asMethodCallExpr();
          if (tryEmitListGetIntoTarget(mc, target, out, indent, ctx)) return;
          if (tryEmitMapGetIntoTarget(mc, target, out, indent, ctx)) return;
        }
      }

      String rhs = expr(val, ctx);
      switch (ae.getOperator()) {
        case ASSIGN -> emit(out, indent, target + " = " + rhs + ".");
        case PLUS -> emit(out, indent, target + " = " + target + " + " + rhs + ".");
        case MINUS -> emit(out, indent, target + " = " + target + " - " + rhs + ".");
        case MULTIPLY -> emit(out, indent, target + " = " + target + " * " + rhs + ".");
        case DIVIDE -> emit(out, indent, target + " = " + target + " / " + rhs + ".");
        default -> emit(out, indent, "\" TODO: assignment op not mapped: " + ae.getOperator());
      }
      return;
    }

    // unary ++/--
    if (e.isUnaryExpr()) {
      UnaryExpr u = e.asUnaryExpr();
      if (u.getOperator() == UnaryExpr.Operator.POSTFIX_INCREMENT ||
          u.getOperator() == UnaryExpr.Operator.PREFIX_INCREMENT) {
        String x = expr(u.getExpression(), ctx);
        emit(out, indent, x + " = " + x + " + 1.");
        return;
      }
      if (u.getOperator() == UnaryExpr.Operator.POSTFIX_DECREMENT ||
          u.getOperator() == UnaryExpr.Operator.PREFIX_DECREMENT) {
        String x = expr(u.getExpression(), ctx);
        emit(out, indent, x + " = " + x + " - 1.");
        return;
      }
    }

    // method call statement
    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();

      // System.out.print/println
      if (isSystemOutPrintln(mc)) {
        String arg = !mc.getArguments().isEmpty() ? expr(mc.getArgument(0), ctx) : "''";
        emit(out, indent, "WRITE: / " + arg + ".");
        return;
      }
      if (isSystemOutPrint(mc)) {
        String arg = !mc.getArguments().isEmpty() ? expr(mc.getArgument(0), ctx) : "''";
        emit(out, indent, "WRITE " + arg + ".");
        return;
      }

      // list.add(x) -> APPEND x TO list.
      if (mc.getScope().isPresent() && mc.getScope().get().isNameExpr()
          && mc.getNameAsString().equals("add") && mc.getArguments().size() == 1) {
        String list = mc.getScope().get().asNameExpr().getNameAsString();
        emit(out, indent, "APPEND " + expr(mc.getArgument(0), ctx) + " TO " + list + ".");
        return;
      }

      // map.put(k,v) -> INSERT VALUE #( key = k value = v ) INTO TABLE m.
      if (mc.getScope().isPresent() && mc.getScope().get().isNameExpr()
          && mc.getNameAsString().equals("put") && mc.getArguments().size() == 2) {
        String map = mc.getScope().get().asNameExpr().getNameAsString();
        emit(out, indent, "INSERT VALUE #( key = " + expr(mc.getArgument(0), ctx) +
            " value = " + expr(mc.getArgument(1), ctx) + " ) INTO TABLE " + map + ".");
        return;
      }

      // generic call without args -> obj->meth( ) / me->meth( ) / zcl_x=>meth( )
      if (mc.getArguments().isEmpty()) {
        emit(out, indent, abapCallNoArgs(mc, ctx));
        return;
      }

      // args: can't map without parameter names => do not emit wrong ABAP
      emit(out, indent, "\" TODO: method call with args not translated (ABAP needs parameter names): " + mc);
      return;
    }

    // standalone new as statement -> skip (needs target)
    if (e.isObjectCreationExpr()) {
      emit(out, indent, "\" TODO: standalone new-expression skipped (needs assignment): " + e);
      return;
    }

    emit(out, indent, "\" TODO: expression not mapped: " + prettyNodeName(e.getClass().getSimpleName()));
  }

  private String abapCallNoArgs(MethodCallExpr mc, Context ctx) {
    String name = mc.getNameAsString();

    if (mc.getScope().isEmpty()) {
      // implicit this in Java
      return (ctx == Context.CLASS ? "me->" : "me->") + name + "( ).";
    }

    Expression scope = mc.getScope().get();
    if (scope.isThisExpr()) return "me->" + name + "( ).";
    if (scope.isSuperExpr()) return "super->" + name + "( ).";

    if (scope.isNameExpr()) {
      String s = scope.asNameExpr().getNameAsString();
      if (looksLikeTypeName(s)) {
        return "zcl_" + toSnakeLower(s) + "=>" + name + "( ).";
      }
      return s + "->" + name + "( ).";
    }

    // field access chain
    return expr(scope, ctx) + "->" + name + "( ).";
  }

  // ============================
  // new operator mappings
  // ============================
  private void emitAbapNewVar(String varName, String declaredJavaType, ObjectCreationExpr oce,
                             StringBuilder out, int indent, Context ctx) {
    String declared = stripGeneric(declaredJavaType);
    String created = stripGeneric(oce.getTypeAsString());

    String type = "var".equals(declared) ? created : declared;
    String abapClass = mapJavaTypeToAbapClass(type);

    // Collections constructed -> table init
    if (looksLikeListType(declaredJavaType) || looksLikeMapType(declaredJavaType) || isCollectionClass(created)) {
      emit(out, indent, "DATA " + varName + " TYPE " + mapTypeForAbap(declaredJavaType) + ".");
      emit(out, indent, "CLEAR " + varName + ".");
      return;
    }

    // We only translate argless constructors safely
    if (!oce.getArguments().isEmpty()) {
      emit(out, indent, "DATA " + varName + " TYPE REF TO " + abapClass + ".");
      emit(out, indent, "\" TODO: NEW " + abapClass + "( ... ) skipped (constructor parameter names unknown)");
      return;
    }

    emit(out, indent, "DATA " + varName + " TYPE REF TO " + abapClass + ".");
    emit(out, indent, varName + " = NEW " + abapClass + "( ).");
  }

  private void emitAbapNewAssign(String target, ObjectCreationExpr oce, StringBuilder out, int indent, Context ctx) {
    String created = stripGeneric(oce.getTypeAsString());
    String abapClass = mapJavaTypeToAbapClass(created);

    if (isCollectionClass(created)) {
      emit(out, indent, "CLEAR " + target + ".");
      return;
    }

    if (!oce.getArguments().isEmpty()) {
      emit(out, indent, "\" TODO: NEW " + abapClass + "( ... ) skipped (constructor parameter names unknown)");
      return;
    }

    emit(out, indent, target + " = NEW " + abapClass + "( ).");
  }

  private boolean isCollectionClass(String t) {
    String x = stripGeneric(t);
    return x.equals("ArrayList") || x.equals("LinkedList") || x.equals("List")
        || x.equals("HashMap") || x.equals("LinkedHashMap") || x.equals("TreeMap") || x.equals("Map");
  }

  // ============================
  // list.get / map.get -> READ TABLE patterns
  // ============================
  private boolean tryEmitListGetIntoTarget(MethodCallExpr mc, String target, StringBuilder out, int indent, Context ctx) {
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

    ensureTemp("lv_idx", "i", out, indent);
    emit(out, indent, "lv_idx = " + expr(idxExpr, ctx) + " + 1.");
    emit(out, indent, "READ TABLE " + list + " INDEX lv_idx INTO " + target + ".");
    emit(out, indent, "IF sy-subrc <> 0. CLEAR " + target + ". ENDIF.");
    return true;
  }

  private boolean tryEmitMapGetIntoTarget(MethodCallExpr mc, String target, StringBuilder out, int indent, Context ctx) {
    if (mc == null) return false;
    if (!mc.getNameAsString().equals("get")) return false;
    if (mc.getScope().isEmpty() || !mc.getScope().get().isNameExpr()) return false;
    if (mc.getArguments().size() != 1) return false;

    String map = mc.getScope().get().asNameExpr().getNameAsString();
    String key = expr(mc.getArgument(0), ctx);

    String entryType = mapVarToEntryType.get(map);
    if (entryType == null) {
      // can't safely type the READ TABLE INTO -> don't emit wrong code
      emit(out, indent, "\" TODO: map.get not translated (map type unknown for '" + map + "')");
      return true;
    }

    String ls = "ls_" + toSnakeLower(map) + "_entry";
    ensureTemp(ls, entryType, out, indent);

    emit(out, indent, "READ TABLE " + map + " WITH TABLE KEY key = " + key + " INTO " + ls + ".");
    emit(out, indent, "IF sy-subrc = 0.");
    emit(out, indent + 2, target + " = " + ls + "-value.");
    emit(out, indent, "ELSE.");
    emit(out, indent + 2, "CLEAR " + target + ".");
    emit(out, indent, "ENDIF.");
    return true;
  }

  private void ensureTemp(String name, String type, StringBuilder out, int indent) {
    if (declaredTemps.contains(name)) return;
    emit(out, indent, "DATA " + name + " TYPE " + type + ".");
    declaredTemps.add(name);
  }

  // ============================
  // for / foreach
  // ============================
  private void translateForEachStmt(ForEachStmt fes, StringBuilder out, int indent, Context ctx) {
    String var = fes.getVariable().getVariable(0).getNameAsString();
    String iterable = expr(fes.getIterable(), ctx);
    emit(out, indent, "LOOP AT " + iterable + " INTO " + var + ".");
    translateStatement(fes.getBody(), out, indent + 2, ctx);
    emit(out, indent, "ENDLOOP.");
  }

  private record ForLoopPattern(String varName, String timesExpr) {}

  private void translateForStmt(ForStmt fs, StringBuilder out, int indent, Context ctx) {
    Optional<ForLoopPattern> pat = detectSimpleCountingLoop(fs);
    if (pat.isPresent()) {
      ForLoopPattern p = pat.get();
      emit(out, indent, "DO " + p.timesExpr + " TIMES.");
      emit(out, indent + 2, p.varName + " = sy-index - 1.");
      translateStatement(fs.getBody(), out, indent + 2, ctx);
      emit(out, indent, "ENDDO.");
      return;
    }
    emit(out, indent, "\" TODO: for-loop not translated (non-trivial).");
    translateStatement(fs.getBody(), out, indent, ctx);
  }

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

    String timesExpr = expr(b.getRight(), Context.SNIPPET);

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

  // ============================
  // SWITCH
  // ============================
  private void translateSwitchStmt(SwitchStmt ss, StringBuilder out, int indent, Context ctx) {
    emit(out, indent, "CASE " + expr(ss.getSelector(), ctx) + ".");
    for (SwitchEntry se : ss.getEntries()) {
      if (se.getLabels().isEmpty()) {
        emit(out, indent, "  WHEN OTHERS.");
      } else {
        List<String> labels = se.getLabels().stream().map(l -> expr(l, ctx)).toList();
        emit(out, indent, "  WHEN " + String.join(" OR ", labels) + ".");
      }
      for (Statement st : se.getStatements()) {
        if (st.isBreakStmt()) continue;
        translateStatement(st, out, indent + 4, ctx);
      }
    }
    emit(out, indent, "ENDCASE.");
  }

  // ============================
  // TRY/CATCH
  // ============================
  private void translateTryStmt(TryStmt ts, StringBuilder out, int indent, Context ctx) {
    emit(out, indent, "TRY.");
    for (Statement st : ts.getTryBlock().getStatements()) translateStatement(st, out, indent + 2, ctx);

    for (CatchClause cc : ts.getCatchClauses()) {
      String var = cc.getParameter().getNameAsString();
      emit(out, indent, "CATCH cx_root INTO " + var + ".");
      emit(out, indent + 2, "\" TODO: Java exception type not mapped: " + cc.getParameter().getTypeAsString());
      for (Statement st : cc.getBody().getStatements()) translateStatement(st, out, indent + 2, ctx);
    }

    if (ts.getFinallyBlock().isPresent()) {
      emit(out, indent, "FINALLY.");
      for (Statement st : ts.getFinallyBlock().get().getStatements()) translateStatement(st, out, indent + 2, ctx);
    }

    emit(out, indent, "ENDTRY.");
  }

  // ============================
  // EXPRESSIONS
  // ============================
  private String expr(Expression e, Context ctx) {
    if (e == null) return "''";

    if (e.isThisExpr()) return "me";
    if (e.isNullLiteralExpr()) return "INITIAL";

    if (e.isNameExpr()) {
      String n = e.asNameExpr().getNameAsString();
      Optional<String> enumConst = resolveEnumConstantUnqualified(n);
      return enumConst.orElse(n);
    }

    if (e.isFieldAccessExpr()) {
      return accessExpr(e.asFieldAccessExpr(), ctx);
    }

    if (e.isIntegerLiteralExpr()) return e.asIntegerLiteralExpr().getValue();
    if (e.isLongLiteralExpr()) return e.asLongLiteralExpr().getValue();
    if (e.isDoubleLiteralExpr()) return e.asDoubleLiteralExpr().getValue();
    if (e.isBooleanLiteralExpr()) return e.asBooleanLiteralExpr().getValue() ? "abap_true" : "abap_false";
    if (e.isStringLiteralExpr()) return "'" + e.asStringLiteralExpr().asString().replace("'", "''") + "'";
    if (e.isCharLiteralExpr()) return "'" + e.asCharLiteralExpr().asChar() + "'";
    if (e.isEnclosedExpr()) return "(" + expr(e.asEnclosedExpr().getInner(), ctx) + ")";

    if (e.isBinaryExpr()) {
      BinaryExpr b = e.asBinaryExpr();

      // null compares -> IS INITIAL
      if ((b.getOperator() == BinaryExpr.Operator.EQUALS || b.getOperator() == BinaryExpr.Operator.NOT_EQUALS) &&
          (b.getLeft().isNullLiteralExpr() || b.getRight().isNullLiteralExpr())) {
        Expression other = b.getLeft().isNullLiteralExpr() ? b.getRight() : b.getLeft();
        boolean notEq = b.getOperator() == BinaryExpr.Operator.NOT_EQUALS;
        return notEq ? (expr(other, ctx) + " IS NOT INITIAL") : (expr(other, ctx) + " IS INITIAL");
      }

      // string concat heuristic
      if (b.getOperator() == BinaryExpr.Operator.PLUS &&
          (b.getLeft().isStringLiteralExpr() || b.getRight().isStringLiteralExpr())) {
        return expr(b.getLeft(), ctx) + " && " + expr(b.getRight(), ctx);
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
      if (op != null) return expr(b.getLeft(), ctx) + " " + op + " " + expr(b.getRight(), ctx);
    }

    if (e.isUnaryExpr()) {
      UnaryExpr u = e.asUnaryExpr();
      if (u.getOperator() == UnaryExpr.Operator.LOGICAL_COMPLEMENT) {
        return "NOT ( " + expr(u.getExpression(), ctx) + " )";
      }
      return expr(u.getExpression(), ctx);
    }

    if (e.isConditionalExpr()) {
      return "COND #( WHEN " + expr(e.asConditionalExpr().getCondition(), ctx) +
          " THEN " + expr(e.asConditionalExpr().getThenExpr(), ctx) +
          " ELSE " + expr(e.asConditionalExpr().getElseExpr(), ctx) + " )";
    }

    if (e.isObjectCreationExpr()) {
      ObjectCreationExpr oce = e.asObjectCreationExpr();
      String created = stripGeneric(oce.getTypeAsString());
      String abapClass = mapJavaTypeToAbapClass(created);
      if (!oce.getArguments().isEmpty()) {
        return "''"; // do not emit wrong inline expression
      }
      return "NEW " + abapClass + "( )";
    }

    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();

      // Common: list.size() -> lines( list )
      if (mc.getScope().isPresent() && mc.getScope().get().isNameExpr()
          && mc.getArguments().isEmpty() && mc.getNameAsString().equals("size")) {
        return "lines( " + mc.getScope().get().asNameExpr().getNameAsString() + " )";
      }

      // list.isEmpty() -> lines( list ) = 0
      if (mc.getScope().isPresent() && mc.getScope().get().isNameExpr()
          && mc.getArguments().isEmpty() && mc.getNameAsString().equals("isEmpty")) {
        return "lines( " + mc.getScope().get().asNameExpr().getNameAsString() + " ) = 0";
      }

      // a.equals(b) -> a = b  (best-effort)
      if (mc.getScope().isPresent() && mc.getArguments().size() == 1 && mc.getNameAsString().equals("equals")) {
        return expr(mc.getScope().get(), ctx) + " = " + expr(mc.getArgument(0), ctx);
      }

      // System.out in expressions -> not meaningful
      if (isSystemOutPrint(mc) || isSystemOutPrintln(mc)) return "''";

      // Everything else: don't invent wrong ABAP
      return "''";
    }

    return "''";
  }

  private String accessExpr(FieldAccessExpr fa, Context ctx) {
    Expression scope = fa.getScope();
    String name = fa.getNameAsString();

    // this.x -> me->x
    if (scope.isThisExpr()) return "me->" + name;

    // Enum constant: Day.FRIDAY -> c_day_friday
    if (scope.isNameExpr()) {
      String left = scope.asNameExpr().getNameAsString();
      Optional<String> enumConst = resolveEnumConstantQualified(left, name);
      if (enumConst.isPresent()) return enumConst.get();

      // static field
      if (looksLikeTypeName(left)) {
        return "zcl_" + toSnakeLower(left) + "=>" + name;
      }
      return left + "->" + name;
    }

    // chained: a.b.c
    if (scope.isFieldAccessExpr()) {
      return accessExpr(scope.asFieldAccessExpr(), ctx) + "->" + name;
    }

    return expr(scope, ctx) + "->" + name;
  }

  // ============================
  // Enums
  // ============================
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

private String resolveVarTypeName(com.github.javaparser.ast.Node node, String varName) {
  // 1) method params + locals
  Optional<MethodDeclaration> md = node.findAncestor(MethodDeclaration.class);
  if (md.isPresent()) {
    for (Parameter p : md.get().getParameters()) {
      if (p.getNameAsString().equals(varName)) return p.getTypeAsString();
    }
    for (VariableDeclarator vd : md.get().findAll(VariableDeclarator.class)) {
      if (vd.getNameAsString().equals(varName)) return vd.getTypeAsString();
    }
  }

  // 2) class fields
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

  private void emitEnumAbapDeclarationsSnippet(StringBuilder out) {
    if (enumConstantsByType.isEmpty()) return;

    for (Map.Entry<String, List<String>> e : enumConstantsByType.entrySet()) {
      String enumType = e.getKey();
      String ty = "ty_" + toSnakeLower(enumType);

      emit(out, 0, "TYPES " + ty + " TYPE string.");
      for (String c : e.getValue()) {
        emit(out, 0, "CONSTANTS c_" + toSnakeLower(enumType) + "_" + toSnakeLower(c) +
            " TYPE " + ty + " VALUE '" + c + "'.");
      }
      emit(out, 0, "");
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

  // ============================
  // List/Map declarations
  // ============================
  private void emitAbapListDecl(String name, String javaType, StringBuilder out, int indent) {
    // List<T> -> STANDARD TABLE OF <T>
    Optional<Generic> g = parseGeneric(javaType);
    String elemType = "string";
    if (g.isPresent() && !g.get().args.isEmpty()) {
      elemType = mapTypeForAbap(g.get().args.get(0));
    }
    emit(out, indent, "DATA " + name + " TYPE STANDARD TABLE OF " + elemType + " WITH EMPTY KEY.");
    listVarToElemType.put(name, elemType);
  }

  private void emitAbapMapDecl(String name, String javaType, StringBuilder out, int indent) {
    // Map<K,V> -> HASHED TABLE OF { key, value }
    Optional<Generic> g = parseGeneric(javaType);
    String k = "string";
    String v = "string";
    if (g.isPresent() && g.get().args.size() >= 2) {
      k = mapTypeForAbap(g.get().args.get(0));
      v = mapTypeForAbap(g.get().args.get(1));
    }
    String ty = "ty_" + toSnakeLower(name) + "_entry";
    emit(out, indent, "TYPES: BEGIN OF " + ty + ",");
    emit(out, indent, "         key   TYPE " + k + ",");
    emit(out, indent, "         value TYPE " + v + ",");
    emit(out, indent, "       END OF " + ty + ".");
    emit(out, indent, "DATA " + name + " TYPE HASHED TABLE OF " + ty + " WITH UNIQUE KEY key.");
    mapVarToEntryType.put(name, ty);
  }

  // ============================
  // Generics parsing
  // ============================
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

  private boolean looksLikeListType(String javaType) {
    String t = stripGeneric(javaType);
    return t.equals("List") || t.equals("ArrayList") || t.equals("LinkedList") || t.equals("Collection");
  }

  private boolean looksLikeMapType(String javaType) {
    String t = stripGeneric(javaType);
    return t.equals("Map") || t.equals("HashMap") || t.equals("LinkedHashMap") || t.equals("TreeMap");
  }

  // ============================
  // Mixed script extraction
  // ============================
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

  // ============================
  // System.out detection
  // ============================
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

  // ============================
  // COMMENTS (Java -> ABAP)
  // ============================
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

  // ============================
  // Helpers / mapping
  // ============================
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
    mapVarToEntryType.clear();
    listVarToElemType.clear();
    declaredTemps.clear();
  }

  private String shortMessage(Exception e) {
    String msg = e.getMessage();
    if (msg == null) return e.getClass().getSimpleName();
    String[] lines = msg.split("\n");
    return lines.length > 0 ? lines[0] : msg;
  }

  private String stripGeneric(String t) {
    if (t == null) return "";
    int lt = t.indexOf('<');
    return lt >= 0 ? t.substring(0, lt).trim() : t.trim();
  }

  private boolean looksLikeTypeName(String n) {
    return n != null && !n.isBlank() && Character.isUpperCase(n.charAt(0));
  }

  private boolean isKnownValueType(String t) {
    return switch (t) {
      case "String", "Integer", "Long", "Double", "Float", "Boolean",
           "int", "long", "double", "float", "boolean", "short", "byte", "char" -> true;
      default -> false;
    };
  }

  private String mapJavaTypeToAbapClass(String javaType) {
    String t = stripGeneric(javaType);
    if (t.isBlank()) return "object";
    return "zcl_" + toSnakeLower(t);
  }

  private String mapTypeForAbap(String javaType) {
    String tRaw = (javaType == null ? "" : javaType.trim());
    String t = stripGeneric(tRaw);

    // arrays
    if (tRaw.endsWith("[]")) {
      String elem = tRaw.substring(0, tRaw.length() - 2);
      return "STANDARD TABLE OF " + mapTypeForAbap(elem) + " WITH EMPTY KEY";
    }

    // enums
    if (enumConstantsByType.containsKey(t)) {
      return "ty_" + toSnakeLower(t);
    }

    // List/Map
    if (looksLikeListType(tRaw)) {
      Optional<Generic> g = parseGeneric(tRaw);
      String elemType = "string";
      if (g.isPresent() && !g.get().args.isEmpty()) elemType = mapTypeForAbap(g.get().args.get(0));
      return "STANDARD TABLE OF " + elemType + " WITH EMPTY KEY";
    }

    if (looksLikeMapType(tRaw)) {
      // maps are declared via emitAbapMapDecl, in signatures we fall back to string to avoid wrong typing
      return "string";
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

  private boolean isSimpleLiteral(Expression e) {
    if (e == null) return false;
    return e.isIntegerLiteralExpr() || e.isBooleanLiteralExpr() || e.isStringLiteralExpr()
        || e.isCharLiteralExpr() || e.isDoubleLiteralExpr() || e.isLongLiteralExpr();
  }
}
