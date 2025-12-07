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

  // ---- per-translation enum info (for enum constants mapping) ----
  private final Map<String, List<String>> enumConstantsByType = new LinkedHashMap<>();
  private final Map<String, List<String>> enumTypesByConstant = new HashMap<>();

  // Reduce comment noise: only emit some TODO once per translation
  private boolean emittedThisMeHint = false;

  public String translateAuto(String javaCode) {
    String src = javaCode == null ? "" : javaCode.trim();
    // If it really looks like a normal Java file (package/import + class/interface/record/enum) AND not mixed script,
    // keep class translation. Otherwise: snippet translation (supports mixed).
    if (looksLikeCompilationUnit(src) && !looksLikeMixedScript(src)) {
      return translateClass(src);
    }
    return translateSnippet(src);
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
  // SNIPPET (supports mixed: enums/classes + statements in one input)
  // Output: ONLY snippet-level ABAP code (no REPORT, no local classes)
  // ============================
  private String translateSnippetInternal(String src) {
    resetState();
    StringBuilder out = new StringBuilder();

    // Extract top-level type decls from anywhere in the snippet, so we can parse mixed input.
    Extracted ex = extractTopLevelTypeDeclsAnywhere(src == null ? "" : src);

    // Wrap to parse: types go into the dummy class body, statements into dummy method body.
    String wrapper =
        "class __J2ABAP {\n" +
            ex.extractedTypes +
            "  void __m() {\n" +
            ex.remainingCode + "\n" +
            "  }\n" +
            "}\n";

    try {
      CompilationUnit cu = StaticJavaParser.parse(wrapper);

      // Index enums and emit their ABAP representation (TYPES + CONSTANTS)
      indexEnums(cu);
      emitEnumAbapDeclarations(out);

      // Find dummy method and translate its statements
      MethodDeclaration m = cu.findFirst(MethodDeclaration.class, md -> md.getNameAsString().equals("__m"))
          .orElse(null);

      if (m == null || m.getBody().isEmpty()) {
        out.append("\" TODO: internal wrapper method missing\n");
        return ensureTrailingNewline(out);
      }

      BlockStmt block = m.getBody().get();
      for (Statement st : block.getStatements()) {
        translateStatement(st, out, 0);
      }

    } catch (Exception e) {
      out.append("Parse error (Java). Tipp: Snippet braucht gültige Statements; gemischte Inputs werden intern gewrappt.\n");
      out.append(shortMessage(e)).append("\n");
    }

    return ensureTrailingNewline(out);
  }

  // ============================
  // CLASS (kept simple; your snippet focus stays clean)
  // ============================
  private String translateClassInternal(String src) {
    resetState();
    StringBuilder out = new StringBuilder();

    try {
      CompilationUnit cu = StaticJavaParser.parse(src == null ? "" : src);

      emitCompilationUnitHeaderComments(cu, out);

      // If it's an enum file, just emit enum ABAP decls
      indexEnums(cu);
      emitEnumAbapDeclarations(out);

      Optional<ClassOrInterfaceDeclaration> clsOpt = cu.findFirst(ClassOrInterfaceDeclaration.class);
      if (clsOpt.isEmpty()) {
        Optional<RecordDeclaration> recOpt = cu.findFirst(RecordDeclaration.class);
        if (recOpt.isPresent()) {
          emit(out, 0, "\" TODO: record translation kept minimal in class-mode");
          return ensureTrailingNewline(out);
        }
        return ensureTrailingNewline(out);
      }

      ClassOrInterfaceDeclaration cls = clsOpt.get();
      String javaName = cls.getNameAsString();
      String abapName = "zcl_" + toSnakeLower(javaName);

      emitLeadingComments(cls, out, 0);

      out.append("CLASS ").append(abapName).append(" DEFINITION PUBLIC FINAL CREATE PUBLIC.\n");
      out.append("  PUBLIC SECTION.\n");

      for (FieldDeclaration fd : cls.getFields()) {
        emitLeadingComments(fd, out, 4);
        for (VariableDeclarator v : fd.getVariables()) {
          String name = v.getNameAsString();
          String type = mapTypeForAbap(v.getTypeAsString());
          emit(out, 4, "DATA " + name + " TYPE " + type + ".");
          v.getInitializer().ifPresent(init -> emit(out, 4, name + " = " + expr(init) + "."));
        }
      }

      for (MethodDeclaration md : cls.getMethods()) {
        emitLeadingComments(md, out, 4);
        out.append("    METHODS ").append(md.getNameAsString()).append(".\n");
      }

      out.append("ENDCLASS.\n");

    } catch (Exception e) {
      out.append("Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/interface/record/enum.\n");
      out.append(shortMessage(e)).append("\n");
    }

    return ensureTrailingNewline(out);
  }

  // ============================
  // STATEMENTS
  // ============================
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

    if (st.isForEachStmt()) {
      translateForEachStmt(st.asForEachStmt(), out, indent);
      return;
    }

    if (st.isForStmt()) {
      translateForStmt(st.asForStmt(), out, indent);
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
      emit(out, indent, "\" TODO: throw -> RAISE EXCEPTION TYPE <cx_...> (mapping unknown)");
      emit(out, indent, "\" thrown: " + expr(ts.getExpression()));
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

    if (st.isReturnStmt()) {
      ReturnStmt rs = st.asReturnStmt();
      if (rs.getExpression().isPresent()) {
        emit(out, indent, "\" TODO: return expr: " + expr(rs.getExpression().get()));
      }
      emit(out, indent, "\" TODO: RETURN (depends on ABAP context)");
      return;
    }

    emit(out, indent, "\" TODO: statement not mapped: " + prettyNodeName(st.getClass().getSimpleName()));
  }

  // ============================
  // EXPRESSION STATEMENTS
  // ============================
  private void translateExpressionStmt(ExpressionStmt es, StringBuilder out, int indent) {
    Expression e = es.getExpression();
    emitLeadingComments(e, out, indent);

    // --- variable declarations ---
    if (e.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = e.asVariableDeclarationExpr();

      for (VariableDeclarator v : vde.getVariables()) {
        String name = v.getNameAsString();
        String javaType = v.getTypeAsString();

        // Map/List with generics -> real ABAP table declaration
        if (looksLikeListType(javaType)) {
          emitAbapListDecl(name, javaType, out, indent);
          v.getInitializer().ifPresent(init -> {
            if (init.isObjectCreationExpr()) {
              // new ArrayList<>() -> just CLEAR (table)
              emit(out, indent, "CLEAR " + name + ".");
            } else {
              emit(out, indent, "\" TODO: list initializer not mapped safely: " + init);
            }
          });
          continue;
        }

        if (looksLikeMapType(javaType)) {
          emitAbapMapDecl(name, javaType, out, indent);
          v.getInitializer().ifPresent(init -> {
            if (init.isObjectCreationExpr()) {
              emit(out, indent, "CLEAR " + name + ".");
            } else {
              emit(out, indent, "\" TODO: map initializer not mapped safely: " + init);
            }
          });
          continue;
        }

        // Enum typed var -> use TYPES ty_<enum> and constants c_<enum>_<const>
        if (enumConstantsByType.containsKey(stripGeneric(javaType))) {
          String ty = "ty_" + toSnakeLower(stripGeneric(javaType));
          emit(out, indent, "DATA " + name + " TYPE " + ty + ".");
          v.getInitializer().ifPresent(init -> emit(out, indent, name + " = " + expr(init) + "."));
          continue;
        }

        // new operator in var init: Auto a = new Auto();
        if (v.getInitializer().isPresent() && v.getInitializer().get().isObjectCreationExpr()) {
          ObjectCreationExpr oce = v.getInitializer().get().asObjectCreationExpr();
          emitAbapNewVar(name, javaType, oce, out, indent);
          continue;
        }

        // Normal
        emit(out, indent, "DATA " + name + " TYPE " + mapTypeForAbap(javaType) + ".");
        if (v.getInitializer().isPresent()) {
          Expression init = v.getInitializer().get();

          // list.get / map.get in initializer -> READ TABLE pattern
          if (init.isMethodCallExpr()) {
            MethodCallExpr mc = init.asMethodCallExpr();
            if (tryEmitListGetIntoTarget(mc, name, out, indent)) continue;
            if (tryEmitMapGetIntoTarget(mc, name, out, indent)) continue;
          }

          emit(out, indent, name + " = " + expr(init) + ".");
        }
      }
      return;
    }

    // --- assignments ---
    if (e.isAssignExpr()) {
      AssignExpr ae = e.asAssignExpr();

      String target = expr(ae.getTarget());
      Expression valExpr = ae.getValue();

      // new in assignment: a = new Auto();
      if (ae.getOperator() == AssignExpr.Operator.ASSIGN && valExpr.isObjectCreationExpr()) {
        emitAbapNewAssign(target, valExpr.asObjectCreationExpr(), out, indent);
        return;
      }

      // map/list get in assignment: v = list.get(0)
      if (ae.getOperator() == AssignExpr.Operator.ASSIGN && valExpr.isMethodCallExpr()) {
        MethodCallExpr mc = valExpr.asMethodCallExpr();
        if (tryEmitListGetIntoTarget(mc, target, out, indent)) return;
        if (tryEmitMapGetIntoTarget(mc, target, out, indent)) return;
      }

      // compound operators
      switch (ae.getOperator()) {
        case ASSIGN -> emit(out, indent, target + " = " + expr(valExpr) + ".");
        case PLUS -> emit(out, indent, target + " = " + target + " + " + expr(valExpr) + ".");
        case MINUS -> emit(out, indent, target + " = " + target + " - " + expr(valExpr) + ".");
        case MULTIPLY -> emit(out, indent, target + " = " + target + " * " + expr(valExpr) + ".");
        case DIVIDE -> emit(out, indent, target + " = " + target + " / " + expr(valExpr) + ".");
        default -> {
          emit(out, indent, "\" TODO: assignment op not mapped: " + ae.getOperator());
          emit(out, indent, target + " = " + expr(valExpr) + ".");
        }
      }
      return;
    }

    // --- unary ++/-- ---
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

    // --- method calls ---
    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();

      // System.out.print/println
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

      // list.add(x) -> APPEND x TO list.
      if (mc.getScope().isPresent() && mc.getScope().get().isNameExpr()
          && mc.getNameAsString().equals("add") && mc.getArguments().size() == 1) {
        String list = mc.getScope().get().asNameExpr().getNameAsString();
        emit(out, indent, "APPEND " + expr(mc.getArgument(0)) + " TO " + list + ".");
        return;
      }

      // map.put(k,v) -> INSERT VALUE #( key = ... value = ... ) INTO TABLE m.
      if (mc.getScope().isPresent() && mc.getScope().get().isNameExpr()
          && mc.getNameAsString().equals("put") && mc.getArguments().size() == 2) {
        String map = mc.getScope().get().asNameExpr().getNameAsString();
        emit(out, indent, "INSERT VALUE #( key = " + expr(mc.getArgument(0)) +
            " value = " + expr(mc.getArgument(1)) + " ) INTO TABLE " + map + ".");
        return;
      }

      // this.foo() -> me->foo( ) (no wrapper emitted; user decides ABAP context)
      if (mc.getScope().isPresent() && mc.getScope().get().isThisExpr() && mc.getArguments().isEmpty()) {
        if (!emittedThisMeHint) {
          emit(out, indent, "\" Hint: me-> ist nur in ABAP-OO (Methoden) gültig.");
          emittedThisMeHint = true;
        }
        emit(out, indent, "me->" + mc.getNameAsString() + "( ).");
        return;
      }

      // object.method() with NO args -> obj->method( ).
      if (mc.getScope().isPresent() && mc.getArguments().isEmpty()) {
        emit(out, indent, expr(mc.getScope().get()) + "->" + mc.getNameAsString() + "( ).");
        return;
      }

      // If args exist: do not guess ABAP parameter names -> comment only
      emit(out, indent, "\" TODO: method call with args not translated (ABAP needs named params): " + mc);
      return;
    }

    // --- standalone new ---
    if (e.isObjectCreationExpr()) {
      emit(out, indent, "\" TODO: standalone new-expression skipped: " + e);
      return;
    }

    emit(out, indent, "\" TODO: expression not mapped: " + prettyNodeName(e.getClass().getSimpleName()));
  }

  // ============================
  // new operator mappings (no class generation!)
  // ============================
  private void emitAbapNewVar(String varName, String declaredJavaType, ObjectCreationExpr oce, StringBuilder out, int indent) {
    String javaType = stripGeneric(declaredJavaType);
    String newType = stripGeneric(oce.getTypeAsString());

    // prefer declared type unless it's 'var'
    String t = "var".equals(javaType) ? newType : javaType;
    String abapClass = mapJavaTypeToAbapClass(t);

    // Collections created via new -> tables, not objects
    if (looksLikeListType(declaredJavaType) || looksLikeMapType(declaredJavaType) || isCollectionClass(newType)) {
      emit(out, indent, "DATA " + varName + " TYPE " + mapTypeForAbap(declaredJavaType) + ".");
      emit(out, indent, "CLEAR " + varName + ".");
      return;
    }

    // Without guessing constructor param names: only translate argless constructors
    if (!oce.getArguments().isEmpty()) {
      emit(out, indent, "DATA " + varName + " TYPE REF TO " + abapClass + ".");
      emit(out, indent, "\" TODO: NEW " + abapClass + "( ... ) skipped (unknown constructor parameter names)");
      emit(out, indent, "CLEAR " + varName + ".");
      return;
    }

    emit(out, indent, "DATA " + varName + " TYPE REF TO " + abapClass + ".");
    emit(out, indent, varName + " = NEW " + abapClass + "( ).");
  }

  private void emitAbapNewAssign(String target, ObjectCreationExpr oce, StringBuilder out, int indent) {
    String newType = stripGeneric(oce.getTypeAsString());
    String abapClass = mapJavaTypeToAbapClass(newType);

    if (isCollectionClass(newType)) {
      emit(out, indent, "CLEAR " + target + ".");
      return;
    }

    if (!oce.getArguments().isEmpty()) {
      emit(out, indent, "\" TODO: assignment NEW " + abapClass + "( ... ) skipped (unknown constructor parameter names)");
      emit(out, indent, "CLEAR " + target + ".");
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
  // list.get / map.get -> READ TABLE patterns (in assignments/initializers)
  // ============================
  private boolean tryEmitListGetIntoTarget(MethodCallExpr mc, String target, StringBuilder out, int indent) {
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

    emit(out, indent, "DATA(lv_idx) = " + expr(idxExpr) + " + 1.");
    emit(out, indent, "READ TABLE " + list + " INDEX lv_idx INTO " + target + ".");
    emit(out, indent, "IF sy-subrc <> 0. CLEAR " + target + ". ENDIF.");
    return true;
  }

  private boolean tryEmitMapGetIntoTarget(MethodCallExpr mc, String target, StringBuilder out, int indent) {
    if (mc == null) return false;
    if (!mc.getNameAsString().equals("get")) return false;
    if (mc.getScope().isEmpty() || !mc.getScope().get().isNameExpr()) return false;
    if (mc.getArguments().size() != 1) return false;

    String map = mc.getScope().get().asNameExpr().getNameAsString();
    String key = expr(mc.getArgument(0));

    emit(out, indent, "READ TABLE " + map + " WITH TABLE KEY key = " + key + " INTO DATA(ls_entry).");
    emit(out, indent, "IF sy-subrc = 0.");
    emit(out, indent + 2, target + " = ls_entry-value.");
    emit(out, indent, "ELSE.");
    emit(out, indent + 2, "CLEAR " + target + ".");
    emit(out, indent, "ENDIF.");
    return true;
  }

  // ============================
  // for / foreach
  // ============================
  private void translateForEachStmt(ForEachStmt fes, StringBuilder out, int indent) {
    String var = fes.getVariable().getVariable(0).getNameAsString();
    String iterable = expr(fes.getIterable());
    emit(out, indent, "LOOP AT " + iterable + " INTO " + var + ".");
    translateStatement(fes.getBody(), out, indent + 2);
    emit(out, indent, "ENDLOOP.");
  }

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
    emit(out, indent, "\" TODO: for-loop not mapped (non-trivial).");
    translateStatement(fs.getBody(), out, indent);
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

    String timesExpr = expr(b.getRight());

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
  private void translateSwitchStmt(SwitchStmt ss, StringBuilder out, int indent) {
    emit(out, indent, "CASE " + expr(ss.getSelector()) + ".");
    for (SwitchEntry se : ss.getEntries()) {
      if (se.getLabels().isEmpty()) {
        emit(out, indent, "  WHEN OTHERS.");
      } else {
        List<String> labels = se.getLabels().stream().map(this::switchLabelExpr).toList();
        emit(out, indent, "  WHEN " + String.join(" OR ", labels) + ".");
      }
      for (Statement st : se.getStatements()) {
        if (st.isBreakStmt()) continue;
        translateStatement(st, out, indent + 4);
      }
    }
    emit(out, indent, "ENDCASE.");
  }

  private String switchLabelExpr(Expression e) {
    if (e == null) return "''";
    if (e.isNameExpr() || e.isFieldAccessExpr()) return expr(e);
    if (e.isStringLiteralExpr()) return "'" + e.asStringLiteralExpr().asString().replace("'", "''") + "'";
    if (e.isIntegerLiteralExpr()) return e.asIntegerLiteralExpr().getValue();
    if (e.isBooleanLiteralExpr()) return e.asBooleanLiteralExpr().getValue() ? "abap_true" : "abap_false";
    return expr(e);
  }

  // ============================
  // TRY/CATCH
  // ============================
  private void translateTryStmt(TryStmt ts, StringBuilder out, int indent) {
    emit(out, indent, "TRY.");
    for (Statement st : ts.getTryBlock().getStatements()) translateStatement(st, out, indent + 2);

    for (CatchClause cc : ts.getCatchClauses()) {
      String var = cc.getParameter().getNameAsString();
      emit(out, indent, "CATCH cx_root INTO DATA(" + var + ").");
      emit(out, indent + 2, "\" TODO: Java catch(" + cc.getParameter().getTypeAsString() + ") -> ABAP mapping unknown");
      for (Statement st : cc.getBody().getStatements()) translateStatement(st, out, indent + 2);
    }

    if (ts.getFinallyBlock().isPresent()) {
      emit(out, indent, "FINALLY.");
      for (Statement st : ts.getFinallyBlock().get().getStatements()) translateStatement(st, out, indent + 2);
    }

    emit(out, indent, "ENDTRY.");
  }

  // ============================
  // EXPRESSIONS
  // ============================
  private String expr(Expression e) {
    if (e == null) return "''";

    if (e.isThisExpr()) return "me";
    if (e.isNullLiteralExpr()) return "INITIAL";

    if (e.isNameExpr()) {
      String n = e.asNameExpr().getNameAsString();
      Optional<String> enumConst = resolveEnumConstantUnqualified(n);
      return enumConst.orElse(n);
    }

    if (e.isFieldAccessExpr()) {
      return accessExpr(e.asFieldAccessExpr());
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

      // null compares -> IS INITIAL
      if ((b.getOperator() == BinaryExpr.Operator.EQUALS || b.getOperator() == BinaryExpr.Operator.NOT_EQUALS) &&
          (b.getLeft().isNullLiteralExpr() || b.getRight().isNullLiteralExpr())) {
        Expression other = b.getLeft().isNullLiteralExpr() ? b.getRight() : b.getLeft();
        boolean notEq = b.getOperator() == BinaryExpr.Operator.NOT_EQUALS;
        return notEq ? (expr(other) + " IS NOT INITIAL") : (expr(other) + " IS INITIAL");
      }

      // string concat heuristic
      if (b.getOperator() == BinaryExpr.Operator.PLUS &&
          (b.getLeft().isStringLiteralExpr() || b.getRight().isStringLiteralExpr())) {
        return expr(b.getLeft()) + " && " + expr(b.getRight());
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
      return expr(u.getExpression());
    }

    if (e.isConditionalExpr()) {
      return "COND #( WHEN " + expr(e.asConditionalExpr().getCondition()) +
          " THEN " + expr(e.asConditionalExpr().getThenExpr()) +
          " ELSE " + expr(e.asConditionalExpr().getElseExpr()) + " )";
    }

    if (e.isObjectCreationExpr()) {
      ObjectCreationExpr oce = e.asObjectCreationExpr();
      String t = stripGeneric(oce.getTypeAsString());
      String abapClass = mapJavaTypeToAbapClass(t);

      if (!oce.getArguments().isEmpty()) {
        return "(* TODO: NEW " + abapClass + "( ... ) not emitted (unknown parameter names) *)";
      }
      return "NEW " + abapClass + "( )";
    }

    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();
      if (isSystemOutPrintln(mc) || isSystemOutPrint(mc)) return "''";
      return "(* TODO: method call expr not inlined *)";
    }

    return "''";
  }

  private String accessExpr(FieldAccessExpr fa) {
    Expression scope = fa.getScope();
    String name = fa.getNameAsString();

    // this.x -> me->x
    if (scope.isThisExpr()) return "me->" + name;

    // Enum constant: Day.FRIDAY -> c_day_friday
    if (scope.isNameExpr()) {
      String left = scope.asNameExpr().getNameAsString();
      Optional<String> enumConst = resolveEnumConstantQualified(left, name);
      if (enumConst.isPresent()) return enumConst.get();

      // static field: ClassName.FIELD -> zcl_classname=>field (best-effort)
      if (looksLikeTypeName(left)) {
        return "zcl_" + toSnakeLower(left) + "=>" + name;
      }

      // otherwise treat as object ref field access
      return left + "->" + name;
    }

    // chained: a.b.c -> a->b->c
    if (scope.isFieldAccessExpr()) {
      return accessExpr(scope.asFieldAccessExpr()) + "->" + name;
    }

    return expr(scope) + "->" + name;
  }

  // ============================
  // Enum declarations emission
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

  private void emitEnumAbapDeclarations(StringBuilder out) {
    if (enumConstantsByType.isEmpty()) return;

    for (Map.Entry<String, List<String>> e : enumConstantsByType.entrySet()) {
      String enumType = e.getKey();
      String ty = "ty_" + toSnakeLower(enumType);

      emit(out, 0, "\" enum " + enumType + " (Java)");
      emit(out, 0, "TYPES " + ty + " TYPE string.");

      for (String c : e.getValue()) {
        emit(out, 0, "CONSTANTS c_" + toSnakeLower(enumType) + "_" + toSnakeLower(c) + " TYPE " + ty + " VALUE '" + c + "'.");
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
  // List/Map declarations (only if user wrote List/Map in Java)
  // ============================
  private void emitAbapListDecl(String name, String javaType, StringBuilder out, int indent) {
    // List<T> -> STANDARD TABLE OF <T>
    Optional<Generic> g = parseGeneric(javaType);
    String elemType = "string";
    if (g.isPresent() && !g.get().args.isEmpty()) {
      elemType = mapTypeForAbap(g.get().args.get(0));
    }
    emit(out, indent, "DATA " + name + " TYPE STANDARD TABLE OF " + elemType + " WITH EMPTY KEY.");
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
  // Mixed script extraction (types anywhere)
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

          // Keep line structure roughly (for error positioning), replace extracted with whitespace/newlines
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
  // Comments (Java -> ABAP)
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

  private String mapJavaTypeToAbapClass(String javaType) {
    // Best-effort: Auto -> zcl_auto
    String t = stripGeneric(javaType);
    if (t.isBlank()) return "object";
    return "zcl_" + toSnakeLower(t);
  }

  private String mapTypeForAbap(String javaType) {
    String t = stripGeneric(javaType == null ? "" : javaType.trim());

    // enum type -> use its ty_<enum> type
    if (enumConstantsByType.containsKey(t)) {
      return "ty_" + toSnakeLower(t);
    }

    // collections
    if (looksLikeListType(javaType)) return "STANDARD TABLE OF string WITH EMPTY KEY";
    if (looksLikeMapType(javaType)) return "string";

    return switch (t) {
      case "int", "Integer", "long", "Long", "short", "Short", "byte", "Byte" -> "i";
      case "double", "Double", "float", "Float" -> "f";
      case "boolean", "Boolean" -> "abap_bool";
      case "String" -> "string";
      default -> {
        // treat unknown type names as REF TO zcl_*
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

  private void resetState() {
    enumConstantsByType.clear();
    enumTypesByConstant.clear();
    emittedThisMeHint = false;
  }

  private boolean looksLikeCompilationUnit(String src) {
    String s = src == null ? "" : src;
    return s.startsWith("package ")
        || s.startsWith("import ")
        || s.contains("\nimport ")
        || s.contains(" class ")
        || s.contains(" interface ")
        || s.contains(" record ")
        || s.contains(" enum ");
  }

  private boolean looksLikeMixedScript(String src) {
    String s = src == null ? "" : src;
    boolean hasType = s.contains("enum ") || s.contains("record ") || s.contains("interface ") || s.contains("class ");
    boolean hasStatements = s.contains(";") || s.contains("if (") || s.contains("while (") || s.contains("for (");
    // enums + top-level statements => mixed
    return hasType && hasStatements && !s.contains("package ");
  }
}
