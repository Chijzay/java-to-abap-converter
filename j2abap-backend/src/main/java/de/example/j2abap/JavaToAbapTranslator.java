package de.example.j2abap;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
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

/**
 * Java -> ABAP best-effort translator (JavaParser-based).
 *
 * Key improvements vs. your original:
 * - Thread-safe: no mutable Spring-singleton fields; per-translation Context object
 * - ABAP ADT classrun output for Java main(): emits IF_OO_ADT_CLASSRUN + out->write(...)
 * - Counting-while optimization:
 *     int j=0; while (j<10){ println(j+1+". Hello!"); j++; }
 *   => DO 10 TIMES. out->write( |{ sy-index }. Hello!| ). ENDDO.
 * - Cleaner emit/format helpers and smaller “TODO” surface
 */
@Service
public class JavaToAbapTranslator {

  // =========================================================
  // Public API
  // =========================================================
  public String translateAuto(String javaCode) {
    String src = normalize(javaCode);
    if (src.isBlank()) return "\n";

    ParseAttempt a = tryParseCompilationUnit(src);
    if (a.ok && a.cu != null && !a.cu.getTypes().isEmpty()) {
      return translateClassInternal(a.fixedSource);
    }
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

  // =========================================================
  // Per-translation context (thread-safe)
  // =========================================================
  private static final class Ctx {
    final Map<String, List<String>> enumConstantsByType = new LinkedHashMap<>();
    final Map<String, List<String>> enumTypesByConstant = new HashMap<>();

    // used only for certain renders (e.g. map j -> ( sy-index - 1 ))
    final Map<String, String> nameSubst = new HashMap<>();

    void resetSubst() { nameSubst.clear(); }
  }

  private enum OutputStyle {
    SNIPPET_LIST,     // WRITE / list output
    CLASSRUN_CONSOLE  // out->write(...)
  }

  // =========================================================
  // SNIPPET
  // =========================================================
  private String translateSnippetInternal(String src) {
    Ctx ctx = new Ctx();
    IndentWriter out = new IndentWriter();

    Extracted ex = extractTopLevelTypeDeclsAnywhere(src);

    if ((ex.remainingCode == null || ex.remainingCode.isBlank())
        && ex.extractedTypes != null && !ex.extractedTypes.isBlank()) {
      out.line(0, "\" Hinweis: „Snippet“ gewählt, aber dein Input enthält nur class/enum/record/interface.");
      out.line(0, "\" Bitte „Class“ wählen oder nur Statements einfügen.");
      return out.finish();
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
      indexEnums(cu, ctx);
      inferEnumsFromSwitches(cu, ctx);

      emitEnumDeclarationsSnippet(out, ctx);

      MethodDeclaration m = cu.findFirst(MethodDeclaration.class, md -> md.getNameAsString().equals("__m"))
          .orElse(null);

      if (m == null || m.getBody().isEmpty()) {
        out.line(0, "\" TODO: keine Statements gefunden");
        return out.finish();
      }

      translateStatementsBlock(
          m.getBody().get().getStatements(),
          out,
          0,
          ctx,
          OutputStyle.SNIPPET_LIST
      );

    } catch (Exception e) {
      out.line(0, "Parse error (Java). Tipp: Snippet braucht gültige Statements.");
      out.line(0, shortMessage(e));
    }

    return out.finish();
  }

  // =========================================================
  // CLASS
  // =========================================================
  private String translateClassInternal(String src) {
    Ctx ctx = new Ctx();
    IndentWriter out = new IndentWriter();

    ParseAttempt a = tryParseCompilationUnit(src);
    if (!a.ok || a.cu == null) {
      out.line(0, "Parse error (Java). Tipp: Class erwartet eine Java-Datei mit class/interface/record/enum.");
      if (a.errorMessage != null) out.line(0, a.errorMessage);
      return out.finish();
    }

    CompilationUnit cu = a.cu;

    emitCompilationUnitHeaderComments(cu, out);

    indexEnums(cu, ctx);
    inferEnumsFromSwitches(cu, ctx);

    Optional<ClassOrInterfaceDeclaration> clsOpt = cu.findFirst(ClassOrInterfaceDeclaration.class);
    Optional<RecordDeclaration> recOpt = cu.findFirst(RecordDeclaration.class);
    Optional<EnumDeclaration> enumOpt = cu.findFirst(EnumDeclaration.class);

    if (clsOpt.isEmpty() && recOpt.isEmpty() && enumOpt.isPresent()) {
      emitEnumDeclarationsSnippet(out, ctx);
      return out.finish();
    }

    if (recOpt.isPresent() && clsOpt.isEmpty()) {
      translateRecordAsClass(recOpt.get(), out, ctx);
      return out.finish();
    }

    if (clsOpt.isEmpty()) {
      out.line(0, "Parse error (Java). Tipp: Keine class/interface/record/enum gefunden.");
      return out.finish();
    }

    ClassOrInterfaceDeclaration cls = clsOpt.get();
    if (cls.isInterface()) {
      translateInterface(cls, out, ctx);
      return out.finish();
    }

    translateClass(cls, out, ctx);
    return out.finish();
  }

  private void translateInterface(ClassOrInterfaceDeclaration itf, IndentWriter out, Ctx ctx) {
    String abapName = "zif_" + toSnakeLower(itf.getNameAsString());
    emitLeadingComments(itf, out, 0);

    out.line(0, "INTERFACE " + abapName + " PUBLIC.");

    for (MethodDeclaration md : itf.getMethods()) {
      emitLeadingComments(md, out, 2);

      StringBuilder sig = new StringBuilder();
      sig.append("  METHODS ").append(md.getNameAsString());

      if (!md.getParameters().isEmpty()) {
        sig.append(" IMPORTING");
        for (Parameter p : md.getParameters()) {
          sig.append(" ").append(p.getNameAsString())
              .append(" TYPE ").append(mapTypeForAbap(p.getTypeAsString(), ctx));
        }
      }

      if (!md.getType().isVoidType()) {
        sig.append(" RETURNING VALUE(rv_result) TYPE ").append(mapTypeForAbap(md.getTypeAsString(), ctx));
      }

      sig.append(".");
      out.line(0, sig.toString());
    }

    out.line(0, "ENDINTERFACE.");
  }

  private void translateRecordAsClass(RecordDeclaration rec, IndentWriter out, Ctx ctx) {
    String abapName = "zcl_" + toSnakeLower(rec.getNameAsString());
    emitLeadingComments(rec, out, 0);

    out.line(0, "CLASS " + abapName + " DEFINITION PUBLIC FINAL CREATE PUBLIC.");
    out.line(2, "PUBLIC SECTION.");

    emitEnumDeclarationsInClassPublic(out, ctx, 4);

    out.line(4, "\" record mapped best-effort: components -> READ-ONLY attributes");
    for (Parameter p : rec.getParameters()) {
      out.line(4, "DATA " + p.getNameAsString() + " TYPE " + mapTypeForAbap(p.getTypeAsString(), ctx) + " READ-ONLY.");
    }

    StringBuilder ctor = new StringBuilder("METHODS constructor");
    if (!rec.getParameters().isEmpty()) {
      ctor.append(" IMPORTING");
      for (Parameter p : rec.getParameters()) {
        ctor.append(" ").append(p.getNameAsString()).append(" TYPE ").append(mapTypeForAbap(p.getTypeAsString(), ctx));
      }
    }
    ctor.append(".");
    out.line(4, ctor.toString());

    out.line(0, "ENDCLASS.");
    out.blank();

    out.line(0, "CLASS " + abapName + " IMPLEMENTATION.");
    out.line(2, "METHOD constructor.");
    for (Parameter p : rec.getParameters()) {
      out.line(4, "me->" + p.getNameAsString() + " = " + p.getNameAsString() + ".");
    }
    out.line(2, "ENDMETHOD.");
    out.line(0, "ENDCLASS.");
  }

  private void translateClass(ClassOrInterfaceDeclaration cls, IndentWriter out, Ctx ctx) {
    String abapName = "zcl_" + toSnakeLower(cls.getNameAsString());
    emitLeadingComments(cls, out, 0);

    MethodDeclaration main = findJavaMain(cls).orElse(null);
    boolean hasMain = (main != null);

    out.line(0, "CLASS " + abapName + " DEFINITION PUBLIC FINAL CREATE PUBLIC.");
    out.line(2, "PUBLIC SECTION.");

    if (hasMain) {
      out.line(4, "INTERFACES if_oo_adt_classrun.");
      out.blank();
    }

    emitEnumDeclarationsInClassPublic(out, ctx, 4);

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
          out.line(4, "CONSTANTS " + name + " TYPE " + mapTypeForAbap(javaType, ctx)
              + " VALUE " + expr(v.getInitializer().get(), ctx) + ".");
          continue;
        }

        String kw = fd.isStatic() ? "CLASS-DATA" : "DATA";
        out.line(4, kw + " " + name + " TYPE " + mapTypeForAbap(javaType, ctx) + ".");
      }
    }

    // Constructor signature (first only)
    List<ConstructorDeclaration> ctors = cls.getConstructors();
    if (!ctors.isEmpty()) {
      ConstructorDeclaration cd = ctors.get(0);
      StringBuilder sig = new StringBuilder("METHODS constructor");
      if (!cd.getParameters().isEmpty()) {
        sig.append(" IMPORTING");
        for (Parameter p : cd.getParameters()) {
          sig.append(" ").append(p.getNameAsString()).append(" TYPE ").append(mapTypeForAbap(p.getTypeAsString(), ctx));
        }
      }
      sig.append(".");
      out.line(4, sig.toString());
    }

    // Methods signatures (skip Java main if we map it to IF_OO_ADT_CLASSRUN~MAIN)
    for (MethodDeclaration md : cls.getMethods()) {
      if (hasMain && md == main) continue;

      emitLeadingComments(md, out, 4);

      String kw = md.isStatic() ? "CLASS-METHODS " : "METHODS ";
      StringBuilder sig = new StringBuilder();
      sig.append(kw).append(md.getNameAsString());

      if (!md.getParameters().isEmpty()) {
        sig.append(" IMPORTING");
        for (Parameter p : md.getParameters()) {
          sig.append(" ").append(p.getNameAsString()).append(" TYPE ").append(mapTypeForAbap(p.getTypeAsString(), ctx));
        }
      }

      if (!md.getType().isVoidType()) {
        sig.append(" RETURNING VALUE(rv_result) TYPE ").append(mapTypeForAbap(md.getTypeAsString(), ctx));
      }

      sig.append(".");
      out.line(4, sig.toString());
    }

    out.line(0, "ENDCLASS.");
    out.blank();

    // IMPLEMENTATION
    out.line(0, "CLASS " + abapName + " IMPLEMENTATION.");

    // Java main -> if_oo_adt_classrun~main
    if (hasMain) {
      out.line(2, "METHOD if_oo_adt_classrun~main.");

      if (main.getBody().isPresent()) {
        translateStatementsBlock(
            main.getBody().get().getStatements(),
            out,
            4,
            ctx,
            OutputStyle.CLASSRUN_CONSOLE
        );
      } else {
        out.line(4, "\" TODO: main body missing");
      }

      out.line(2, "ENDMETHOD.");
      out.blank();
    }

    // Constructor body
    if (!ctors.isEmpty()) {
      ConstructorDeclaration cd = ctors.get(0);
      out.line(2, "METHOD constructor.");
      translateStatementsBlock(cd.getBody().getStatements(), out, 4, ctx, OutputStyle.CLASSRUN_CONSOLE);
      out.line(2, "ENDMETHOD.");
      out.blank();

      if (ctors.size() > 1) {
        out.line(2, "\" TODO: mehrere Java-Konstruktoren; ABAP hat kein Overload (nur der erste wurde übersetzt).");
        out.blank();
      }
    }

    // Methods bodies
    for (MethodDeclaration md : cls.getMethods()) {
      if (hasMain && md == main) continue;

      out.line(2, "METHOD " + md.getNameAsString() + ".");

      if (md.getBody().isPresent()) {
        translateStatementsBlock(md.getBody().get().getStatements(), out, 4, ctx, OutputStyle.CLASSRUN_CONSOLE);
      } else {
        out.line(4, "\" TODO: method body missing");
      }

      out.line(2, "ENDMETHOD.");
      out.blank();
    }

    out.line(0, "ENDCLASS.");
  }

  // =========================================================
  // Main feature: Statement-block translation + optimization
  // =========================================================
  private void translateStatementsBlock(List<Statement> stmts, IndentWriter out, int indent, Ctx ctx, OutputStyle style) {
    if (stmts == null || stmts.isEmpty()) return;

    for (int i = 0; i < stmts.size(); ) {
      int consumed = tryEmitCountingWhileAsDo(stmts, i, out, indent, ctx, style);
      if (consumed > 0) {
        i += consumed;
        continue;
      }

      translateStatement(stmts.get(i), out, indent, ctx, style);
      i++;
    }
  }

  /**
   * Pattern:
   *   int j = 0;
   *   while (j < 10) {
   *     System.out.println(j + 1 + ". Hello!");
   *     j++;
   *   }
   *
   * -> DO 10 TIMES. out->write( |{ sy-index }. Hello!| ). ENDDO.
   *
   * Returns how many original statements were consumed (0 = no match).
   */
  private int tryEmitCountingWhileAsDo(List<Statement> stmts, int idx, IndentWriter out, int indent, Ctx ctx, OutputStyle style) {
    if (style != OutputStyle.CLASSRUN_CONSOLE) return 0; // keep list-output simple

    if (idx + 1 >= stmts.size()) return 0;

    CounterInit init = parseCounterInit(stmts.get(idx));
    if (init == null) return 0;

    Statement s1 = stmts.get(idx + 1);
    if (!s1.isWhileStmt()) return 0;

    WhileStmt ws = s1.asWhileStmt();

    // counter must not be used later (otherwise we can't drop it)
    if (isNameUsedInAny(stmts.subList(idx + 2, stmts.size()), init.varName)) return 0;

    // condition: j < N  OR j <= N
    LoopBound bound = parseWhileBound(ws.getCondition(), init.varName);
    if (bound == null) return 0;

    // body must contain: (optional) print + (required) increment and nothing else significant
    BlockStmt body = ws.getBody().isBlockStmt()
        ? ws.getBody().asBlockStmt()
        : new BlockStmt(List.of(ws.getBody()));

    List<Statement> bodyStmts = body.getStatements();
    if (bodyStmts.isEmpty()) return 0;

    // detect increment statement (prefer last)
    int incPos = findIncrementStatement(bodyStmts, init.varName);
    if (incPos < 0) return 0;

    // Only allow: print statements (0..n but realistically 1) + the increment; no other complex stuff
    List<Statement> others = new ArrayList<>();
    for (int i = 0; i < bodyStmts.size(); i++) {
      if (i == incPos) continue;
      Statement st = bodyStmts.get(i);
      if (isPurePrintln(st)) continue;
      if (st.isEmptyStmt()) continue;
      // allow comments-only blocks implicitly (comments are on nodes)
      others.add(st);
    }
    if (!others.isEmpty()) return 0;

    // compute times (only for constant bound)
    Integer times = computeTimes(init.startValue, bound);
    if (times == null || times <= 0) return 0;

    // render the (first) println argument nicely with sy-index if possible
    MethodCallExpr printCall = extractFirstPrintCall(bodyStmts);
    String printArgAbap = "''";
    if (printCall != null) {
      Expression arg = printCall.getArguments().isEmpty() ? null : printCall.getArgument(0);
      String pretty = tryRenderPrintWithSyIndex(arg, init.varName, init.startValue, ctx);
      if (pretty != null) {
        printArgAbap = pretty;
      } else {
        // fallback: render with substitution var -> ( start + sy-index - 1 )
        ctx.resetSubst();
        ctx.nameSubst.put(init.varName, loopVarAbap(init.startValue));
        printArgAbap = renderString(arg, ctx, OutputStyle.CLASSRUN_CONSOLE);
        ctx.resetSubst();
      }
    }

    out.line(indent, "DO " + times + " TIMES.");
    out.line(indent + 2, "out->write( " + printArgAbap + " ).");
    out.line(indent, "ENDDO.");

    return 2; // consumed init + while
  }

  private record CounterInit(String varName, int startValue) {}

  private CounterInit parseCounterInit(Statement st) {
    if (st == null) return null;

    // int j = 0;
    if (st.isExpressionStmt() && st.asExpressionStmt().getExpression().isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = st.asExpressionStmt().getExpression().asVariableDeclarationExpr();
      if (vde.getVariables().size() != 1) return null;
      VariableDeclarator v = vde.getVariable(0);
      if (!v.getInitializer().isPresent()) return null;
      if (!isIntLike(v.getTypeAsString())) return null;
      Expression init = v.getInitializer().get();
      if (!init.isIntegerLiteralExpr()) return null;
      return new CounterInit(v.getNameAsString(), Integer.parseInt(init.asIntegerLiteralExpr().getValue()));
    }

    // j = 0;
    if (st.isExpressionStmt() && st.asExpressionStmt().getExpression().isAssignExpr()) {
      AssignExpr ae = st.asExpressionStmt().getExpression().asAssignExpr();
      if (ae.getOperator() != AssignExpr.Operator.ASSIGN) return null;
      if (!ae.getTarget().isNameExpr()) return null;
      if (!ae.getValue().isIntegerLiteralExpr()) return null;
      return new CounterInit(ae.getTarget().asNameExpr().getNameAsString(),
          Integer.parseInt(ae.getValue().asIntegerLiteralExpr().getValue()));
    }

    return null;
  }

  private boolean isIntLike(String t) {
    String x = stripGeneric(t);
    return x.equals("int") || x.equals("Integer") || x.equals("short") || x.equals("Short") || x.equals("byte") || x.equals("Byte");
  }

  private record LoopBound(BinaryExpr.Operator op, int rightLiteral) {}

  private LoopBound parseWhileBound(Expression cond, String varName) {
    if (cond == null) return null;
    if (cond.isEnclosedExpr()) cond = cond.asEnclosedExpr().getInner();

    if (!cond.isBinaryExpr()) return null;
    BinaryExpr b = cond.asBinaryExpr();

    if (!(b.getOperator() == BinaryExpr.Operator.LESS || b.getOperator() == BinaryExpr.Operator.LESS_EQUALS)) return null;

    if (!b.getLeft().isNameExpr()) return null;
    if (!b.getLeft().asNameExpr().getNameAsString().equals(varName)) return null;

    Expression r = b.getRight();
    if (r.isEnclosedExpr()) r = r.asEnclosedExpr().getInner();
    if (!r.isIntegerLiteralExpr()) return null;

    int lit = Integer.parseInt(r.asIntegerLiteralExpr().getValue());
    return new LoopBound(b.getOperator(), lit);
  }

  private Integer computeTimes(int start, LoopBound bound) {
    // Only handle ascending +1 loops (j++)
    // while (j < N) => N - start
    // while (j <= N) => N - start + 1
    if (bound.op == BinaryExpr.Operator.LESS) {
      return bound.rightLiteral - start;
    } else if (bound.op == BinaryExpr.Operator.LESS_EQUALS) {
      return bound.rightLiteral - start + 1;
    }
    return null;
  }

  private int findIncrementStatement(List<Statement> bodyStmts, String varName) {
    // prefer last, but accept anywhere
    for (int i = bodyStmts.size() - 1; i >= 0; i--) {
      if (isIncrementOf(bodyStmts.get(i), varName)) return i;
    }
    return -1;
  }

  private boolean isIncrementOf(Statement st, String varName) {
    if (st == null) return false;
    if (!st.isExpressionStmt()) return false;
    Expression e = st.asExpressionStmt().getExpression();

    // j++; ++j;
    if (e.isUnaryExpr()) {
      UnaryExpr u = e.asUnaryExpr();
      if (!u.getExpression().isNameExpr()) return false;
      if (!u.getExpression().asNameExpr().getNameAsString().equals(varName)) return false;
      return u.getOperator() == UnaryExpr.Operator.POSTFIX_INCREMENT || u.getOperator() == UnaryExpr.Operator.PREFIX_INCREMENT;
    }

    // j += 1;
    if (e.isAssignExpr()) {
      AssignExpr ae = e.asAssignExpr();
      if (!ae.getTarget().isNameExpr()) return false;
      if (!ae.getTarget().asNameExpr().getNameAsString().equals(varName)) return false;

      if (ae.getOperator() == AssignExpr.Operator.PLUS) {
        return ae.getValue().isIntegerLiteralExpr() && ae.getValue().asIntegerLiteralExpr().getValue().equals("1");
      }

      // j = j + 1;
      if (ae.getOperator() == AssignExpr.Operator.ASSIGN && ae.getValue().isBinaryExpr()) {
        BinaryExpr b = ae.getValue().asBinaryExpr();
        if (b.getOperator() != BinaryExpr.Operator.PLUS) return false;
        return isName(b.getLeft(), varName) && isIntLiteral(b.getRight(), 1)
            || isName(b.getRight(), varName) && isIntLiteral(b.getLeft(), 1);
      }
    }

    return false;
  }

  private boolean isPurePrintln(Statement st) {
    if (st == null) return false;
    if (!st.isExpressionStmt()) return false;
    Expression e = st.asExpressionStmt().getExpression();
    if (!e.isMethodCallExpr()) return false;
    MethodCallExpr mc = e.asMethodCallExpr();
    return isSystemOutPrintln(mc) || isSystemOutPrint(mc);
  }

  private MethodCallExpr extractFirstPrintCall(List<Statement> bodyStmts) {
    for (Statement st : bodyStmts) {
      if (!st.isExpressionStmt()) continue;
      Expression e = st.asExpressionStmt().getExpression();
      if (!e.isMethodCallExpr()) continue;
      MethodCallExpr mc = e.asMethodCallExpr();
      if (isSystemOutPrintln(mc) || isSystemOutPrint(mc)) return mc;
    }
    return null;
  }

  private boolean isName(Expression e, String name) {
    return e != null && e.isNameExpr() && e.asNameExpr().getNameAsString().equals(name);
  }

  private boolean isIntLiteral(Expression e, int v) {
    return e != null && e.isIntegerLiteralExpr() && Integer.parseInt(e.asIntegerLiteralExpr().getValue()) == v;
  }

  private boolean isNameUsedInAny(List<Statement> stmts, String varName) {
    for (Statement s : stmts) {
      if (s == null) continue;
      if (s.findAll(NameExpr.class).stream().anyMatch(ne -> ne.getNameAsString().equals(varName))) return true;
    }
    return false;
  }

  private String loopVarAbap(int start) {
    // value of j inside DO loop iteration k:
    // j = start + (sy-index - 1)
    if (start == 0) return "( sy-index - 1 )";
    if (start == 1) return "sy-index";
    return "( " + start + " + sy-index - 1 )";
  }

  private String tryRenderPrintWithSyIndex(Expression arg, String varName, int start, Ctx ctx) {
    if (arg == null) return "''";

    // Only “pretty” for the most common case start==0 and a concat that begins with (j+1)
    if (start != 0) return null;

    List<ConcatSeg> segs = flattenJavaConcat(arg);
    if (segs == null || segs.isEmpty()) return null;

    // first segment must be exactly (j + 1) or (1 + j)
    ConcatSeg first = segs.get(0);
    if (first.kind != ConcatSeg.Kind.EXPR) return null;
    if (!isVarPlusOne(first.expr, varName)) return null;

    // Build ABAP string template: |{ sy-index }...|
    StringBuilder tpl = new StringBuilder();
    tpl.append("|{ sy-index }");

    for (int i = 1; i < segs.size(); i++) {
      ConcatSeg s = segs.get(i);
      if (s.kind == ConcatSeg.Kind.TEXT) {
        tpl.append(escapeForAbapStringTemplate(s.text));
      } else {
        // embed expression with j mapped to ( sy-index - 1 )
        ctx.resetSubst();
        ctx.nameSubst.put(varName, "( sy-index - 1 )");
        String e = expr(s.expr, ctx);
        ctx.resetSubst();
        tpl.append("{ ").append(e).append(" }");
      }
    }

    tpl.append("|");
    return tpl.toString();
  }

  private boolean isVarPlusOne(Expression e, String varName) {
    if (e == null) return false;
    if (e.isEnclosedExpr()) e = e.asEnclosedExpr().getInner();
    if (!e.isBinaryExpr()) return false;
    BinaryExpr b = e.asBinaryExpr();
    if (b.getOperator() != BinaryExpr.Operator.PLUS) return false;

    Expression l = b.getLeft();
    Expression r = b.getRight();
    if (l.isEnclosedExpr()) l = l.asEnclosedExpr().getInner();
    if (r.isEnclosedExpr()) r = r.asEnclosedExpr().getInner();

    return (isName(l, varName) && isIntLiteral(r, 1)) || (isName(r, varName) && isIntLiteral(l, 1));
  }

  private String escapeForAbapStringTemplate(String s) {
    // ABAP string templates are delimited by | ... |
    // escape literal | by doubling it: ||
    if (s == null) return "";
    return s.replace("|", "||");
  }

  private static final class ConcatSeg {
    enum Kind { TEXT, EXPR }
    final Kind kind;
    final String text;
    final Expression expr;

    private ConcatSeg(Kind kind, String text, Expression expr) {
      this.kind = kind;
      this.text = text;
      this.expr = expr;
    }
    static ConcatSeg text(String t) { return new ConcatSeg(Kind.TEXT, t, null); }
    static ConcatSeg expr(Expression e) { return new ConcatSeg(Kind.EXPR, null, e); }
  }

  /**
   * Flattens Java string concatenation chains: a + b + "x" + c ...
   * Returns segments where string literals become TEXT, everything else becomes EXPR.
   */
  private List<ConcatSeg> flattenJavaConcat(Expression e) {
    if (e == null) return List.of();

    if (e.isEnclosedExpr()) return flattenJavaConcat(e.asEnclosedExpr().getInner());

    if (e.isBinaryExpr() && e.asBinaryExpr().getOperator() == BinaryExpr.Operator.PLUS) {
      BinaryExpr b = e.asBinaryExpr();
      List<ConcatSeg> left = flattenJavaConcat(b.getLeft());
      List<ConcatSeg> right = flattenJavaConcat(b.getRight());
      List<ConcatSeg> out = new ArrayList<>(left.size() + right.size());
      out.addAll(left);
      out.addAll(right);
      return out;
    }

    if (e.isStringLiteralExpr()) return List.of(ConcatSeg.text(e.asStringLiteralExpr().asString()));
    if (e.isCharLiteralExpr()) return List.of(ConcatSeg.text(String.valueOf(e.asCharLiteralExpr().asChar())));

    return List.of(ConcatSeg.expr(e));
  }

  // =========================================================
  // Statements
  // =========================================================
  private void translateStatement(Statement st, IndentWriter out, int indent, Ctx ctx, OutputStyle style) {
    if (st == null) return;

    emitLeadingComments(st, out, indent);

    if (st.isBlockStmt()) {
      translateStatementsBlock(st.asBlockStmt().getStatements(), out, indent, ctx, style);
      return;
    }
    if (st.isEmptyStmt()) return;

    if (st.isExpressionStmt()) {
      translateExpressionStmt(st.asExpressionStmt(), out, indent, ctx, style);
      return;
    }

    if (st.isIfStmt()) {
      IfStmt is = st.asIfStmt();
      ExprRes cond = tryExpr(is.getCondition(), ctx);
      if (!cond.ok) { emitTodoJava(out, indent, st.toString(), cond.reason); return; }

      out.line(indent, "IF " + cond.abap + ".");
      translateStatement(is.getThenStmt(), out, indent + 2, ctx, style);
      if (is.getElseStmt().isPresent()) {
        out.line(indent, "ELSE.");
        translateStatement(is.getElseStmt().get(), out, indent + 2, ctx, style);
      }
      out.line(indent, "ENDIF.");
      return;
    }

    if (st.isSwitchStmt()) {
      translateSwitchStmt(st.asSwitchStmt(), out, indent, ctx, style);
      return;
    }

    if (st.isTryStmt()) {
      translateTryStmt(st.asTryStmt(), out, indent, ctx, style);
      return;
    }

    if (st.isWhileStmt()) {
      WhileStmt ws = st.asWhileStmt();
      ExprRes cond = tryExpr(ws.getCondition(), ctx);
      if (!cond.ok) { emitTodoJava(out, indent, st.toString(), cond.reason); return; }

      out.line(indent, "WHILE " + cond.abap + ".");
      translateStatement(ws.getBody(), out, indent + 2, ctx, style);
      out.line(indent, "ENDWHILE.");
      return;
    }

    if (st.isForEachStmt()) {
      ForEachStmt fes = st.asForEachStmt();
      String var = fes.getVariable().getVariable(0).getNameAsString();
      ExprRes it = tryExpr(fes.getIterable(), ctx);
      if (!it.ok) { emitTodoJava(out, indent, st.toString(), it.reason); return; }

      out.line(indent, "LOOP AT " + it.abap + " INTO " + var + ".");
      translateStatement(fes.getBody(), out, indent + 2, ctx, style);
      out.line(indent, "ENDLOOP.");
      return;
    }

    if (st.isReturnStmt()) {
      ReturnStmt rs = st.asReturnStmt();
      if (rs.getExpression().isPresent()) {
        ExprRes er = tryExpr(rs.getExpression().get(), ctx);
        if (!er.ok) { emitTodoJava(out, indent, st.toString(), er.reason); return; }
        out.line(indent, "rv_result = " + er.abap + ".");
      }
      out.line(indent, "RETURN.");
      return;
    }

    if (st.isBreakStmt()) { out.line(indent, "EXIT."); return; }
    if (st.isContinueStmt()) { out.line(indent, "CONTINUE."); return; }

    emitTodoJava(out, indent, st.toString(), "Statement nicht unterstützt / nicht eindeutig in ABAP");
  }

  private void translateTryStmt(TryStmt ts, IndentWriter out, int indent, Ctx ctx, OutputStyle style) {
    out.line(indent, "TRY.");
    translateStatementsBlock(ts.getTryBlock().getStatements(), out, indent + 2, ctx, style);

    for (CatchClause cc : ts.getCatchClauses()) {
      String var = cc.getParameter().getNameAsString();
      out.line(indent, "CATCH cx_root INTO DATA(" + var + ").");
      out.line(indent + 2, "\" TODO: Java exception type nicht gemappt: " + cc.getParameter().getTypeAsString());
      translateStatementsBlock(cc.getBody().getStatements(), out, indent + 2, ctx, style);
    }

    if (ts.getFinallyBlock().isPresent()) {
      out.line(indent, "FINALLY.");
      translateStatementsBlock(ts.getFinallyBlock().get().getStatements(), out, indent + 2, ctx, style);
    }

    out.line(indent, "ENDTRY.");
  }

  private void translateSwitchStmt(SwitchStmt ss, IndentWriter out, int indent, Ctx ctx, OutputStyle style) {
    ExprRes selector = tryExpr(ss.getSelector(), ctx);
    if (!selector.ok) { emitTodoJava(out, indent, ss.toString(), selector.reason); return; }

    out.line(indent, "CASE " + selector.abap + ".");

    List<SwitchEntry> entries = ss.getEntries();
    int i = 0;
    while (i < entries.size()) {
      SwitchEntry entry = entries.get(i);

      boolean isDefault = entry.getLabels().isEmpty();
      List<String> labels = new ArrayList<>();
      if (!isDefault) labels.addAll(entry.getLabels().stream().map(e -> switchLabelExpr(e, ctx)).toList());

      List<Statement> stmts = new ArrayList<>(entry.getStatements());
      int j = i;
      while (stmts.isEmpty() && j + 1 < entries.size()) {
        SwitchEntry next = entries.get(j + 1);
        if (next.getLabels().isEmpty()) {
          isDefault = true;
        } else {
          labels.addAll(next.getLabels().stream().map(e -> switchLabelExpr(e, ctx)).toList());
        }
        stmts = new ArrayList<>(next.getStatements());
        j++;
        if (!stmts.isEmpty()) break;
      }

      if (isDefault || labels.isEmpty()) {
        out.line(indent, "  WHEN OTHERS.");
      } else {
        out.line(indent, "  WHEN " + String.join(" OR ", labels) + ".");
      }

      for (Statement st : stmts) {
        if (st.isBreakStmt()) continue;
        translateStatement(st, out, indent + 4, ctx, style);
      }

      i = j + 1;
    }

    out.line(indent, "ENDCASE.");
  }

  private String switchLabelExpr(Expression e, Ctx ctx) {
    if (e == null) return "''";

    if (e.isNameExpr()) {
      String n = e.asNameExpr().getNameAsString();
      return resolveEnumConstantUnqualified(n, ctx).orElse(n);
    }

    if (e.isFieldAccessExpr()) {
      FieldAccessExpr fa = e.asFieldAccessExpr();
      if (fa.getScope().isNameExpr()) {
        return resolveEnumConstantQualified(
            fa.getScope().asNameExpr().getNameAsString(),
            fa.getNameAsString(),
            ctx
        ).orElse(accessExpr(fa, ctx));
      }
      return accessExpr(fa, ctx);
    }

    ExprRes er = tryExpr(e, ctx);
    return er.ok ? er.abap : "''";
  }

  // =========================================================
  // Expression statements
  // =========================================================
  private void translateExpressionStmt(ExpressionStmt es, IndentWriter out, int indent, Ctx ctx, OutputStyle style) {
    Expression e = es.getExpression();
    emitLeadingComments(e, out, indent);

    // Variable declarations
    if (e.isVariableDeclarationExpr()) {
      VariableDeclarationExpr vde = e.asVariableDeclarationExpr();
      for (VariableDeclarator v : vde.getVariables()) {
        translateVarDeclarator(v, out, indent, ctx);
      }
      return;
    }

    // Assignments
    if (e.isAssignExpr()) {
      translateAssignExpr(e.asAssignExpr(), out, indent, ctx);
      return;
    }

    // Method call as statement
    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();

      // System.out.println / print
      if (isSystemOutPrintln(mc) || isSystemOutPrint(mc)) {
        Expression arg0 = mc.getArguments().isEmpty() ? null : mc.getArgument(0);
        if (style == OutputStyle.CLASSRUN_CONSOLE) {
          out.line(indent, "out->write( " + renderString(arg0, ctx, style) + " ).");
        } else {
          out.line(indent, "WRITE: / " + expr(arg0, ctx) + ".");
        }
        return;
      }

      // no-arg calls -> translate as instance/class call
      if (mc.getArguments().isEmpty()) {
        out.line(indent, abapCallNoArgs(mc, ctx));
        return;
      }

      emitTodoJava(out, indent, es.toString(), "Method call mit Argumenten nicht eindeutig (ABAP braucht Parameternamen)");
      return;
    }

    // new as standalone statement -> TODO
    if (e.isObjectCreationExpr()) {
      emitTodoJava(out, indent, es.toString(), "'new' als Statement ohne Zuweisung");
      return;
    }

    emitTodoJava(out, indent, es.toString(), "Expression nicht unterstützt / nicht eindeutig");
  }

  private void translateVarDeclarator(VariableDeclarator v, IndentWriter out, int indent, Ctx ctx) {
    String name = v.getNameAsString();
    String javaType = v.getTypeAsString();

    if (v.getInitializer().isPresent() && v.getInitializer().get().isObjectCreationExpr()) {
      ObjectCreationExpr oce = v.getInitializer().get().asObjectCreationExpr();
      emitNewVar(name, javaType, oce, out, indent, ctx);
      return;
    }

    out.line(indent, "DATA " + name + " TYPE " + mapTypeForAbap(javaType, ctx) + ".");

    if (v.getInitializer().isPresent()) {
      ExprRes er = tryExpr(v.getInitializer().get(), ctx);
      if (er.ok) out.line(indent, name + " = " + er.abap + ".");
      else emitTodoJava(out, indent, v.toString() + ";", er.reason);
    }
  }

  private void translateAssignExpr(AssignExpr ae, IndentWriter out, int indent, Ctx ctx) {
    Expression targetExpr = ae.getTarget();
    Expression valueExpr = ae.getValue();

    // i = i++; etc.
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

        String t = expr(targetExpr, ctx);
        out.line(indent, "\" TODO: Java \"" + ae + "\" ist trickreich; Effekt: Wert bleibt gleich.");
        out.line(indent, t + " = " + t + ".");
        return;
      }
    }

    // new assignment
    if (ae.getOperator() == AssignExpr.Operator.ASSIGN && valueExpr.isObjectCreationExpr()) {
      emitNewAssign(expr(targetExpr, ctx), valueExpr.asObjectCreationExpr(), out, indent, ctx);
      return;
    }

    String target = expr(targetExpr, ctx);

    if (ae.getOperator() != AssignExpr.Operator.ASSIGN) {
      ExprRes rhs = tryExpr(valueExpr, ctx);
      if (!rhs.ok) { emitTodoJava(out, indent, ae + ";", rhs.reason); return; }

      switch (ae.getOperator()) {
        case PLUS -> out.line(indent, target + " = " + target + " + " + rhs.abap + ".");
        case MINUS -> out.line(indent, target + " = " + target + " - " + rhs.abap + ".");
        case MULTIPLY -> out.line(indent, target + " = " + target + " * " + rhs.abap + ".");
        case DIVIDE -> out.line(indent, target + " = " + target + " / " + rhs.abap + ".");
        default -> emitTodoJava(out, indent, ae + ";", "compound assignment nicht gemappt: " + ae.getOperator());
      }
      return;
    }

    ExprRes rhs = tryExpr(valueExpr, ctx);
    if (!rhs.ok) { emitTodoJava(out, indent, ae + ";", rhs.reason); return; }
    out.line(indent, target + " = " + rhs.abap + ".");
  }

  // =========================================================
  // Expression translation
  // =========================================================
  private record ExprRes(boolean ok, String abap, String reason) {}

  private ExprRes tryExpr(Expression e, Ctx ctx) {
    try {
      return new ExprRes(true, expr(e, ctx), null);
    } catch (UnsupportedExpression ex) {
      return new ExprRes(false, "''", ex.getMessage());
    } catch (Exception ex) {
      return new ExprRes(false, "''", "Expression nicht übersetzbar");
    }
  }

  private static final class UnsupportedExpression extends RuntimeException {
    UnsupportedExpression(String msg) { super(msg); }
  }

  private String renderString(Expression e, Ctx ctx, OutputStyle style) {
    if (e == null) return "''";

    // For concat chains we produce ABAP string templates |...|
    List<ConcatSeg> segs = flattenJavaConcat(e);
    boolean hasText = segs.stream().anyMatch(s -> s.kind == ConcatSeg.Kind.TEXT);
    boolean hasMany = segs.size() > 1;

    if (hasText || hasMany) {
      StringBuilder tpl = new StringBuilder();
      tpl.append("|");
      for (ConcatSeg s : segs) {
        if (s.kind == ConcatSeg.Kind.TEXT) {
          tpl.append(escapeForAbapStringTemplate(s.text));
        } else {
          tpl.append("{ ").append(expr(s.expr, ctx)).append(" }");
        }
      }
      tpl.append("|");
      return tpl.toString();
    }

    // otherwise: if it's a string literal => '...'
    if (e.isStringLiteralExpr()) return "'" + e.asStringLiteralExpr().asString().replace("'", "''") + "'";
    if (e.isCharLiteralExpr()) return "'" + String.valueOf(e.asCharLiteralExpr().asChar()).replace("'", "''") + "'";

    // last resort: embed in template (safe for console)
    return "|{ " + expr(e, ctx) + " }|";
  }

  private String expr(Expression e, Ctx ctx) {
    if (e == null) return "''";

    if (e.isEnclosedExpr()) return "(" + expr(e.asEnclosedExpr().getInner(), ctx) + ")";

    if (e.isThisExpr()) return "me";
    if (e.isSuperExpr()) return "super";
    if (e.isNullLiteralExpr()) return "INITIAL";

    if (e.isNameExpr()) {
      String n = e.asNameExpr().getNameAsString();
      if (ctx.nameSubst.containsKey(n)) return ctx.nameSubst.get(n);
      return resolveEnumConstantUnqualified(n, ctx).orElse(n);
    }

    if (e.isFieldAccessExpr()) {
      FieldAccessExpr fa = e.asFieldAccessExpr();

      if (fa.getScope().isThisExpr()) return "me->" + fa.getNameAsString();

      if (fa.getScope().isNameExpr()) {
        String left = fa.getScope().asNameExpr().getNameAsString();
        Optional<String> enumConst = resolveEnumConstantQualified(left, fa.getNameAsString(), ctx);
        if (enumConst.isPresent()) return enumConst.get();

        if (looksLikeTypeName(left)) return "zcl_" + toSnakeLower(left) + "=>" + fa.getNameAsString();

        return left + "->" + fa.getNameAsString();
      }

      if (fa.getScope().isFieldAccessExpr()) {
        return accessExpr(fa.getScope().asFieldAccessExpr(), ctx) + "->" + fa.getNameAsString();
      }

      return expr(fa.getScope(), ctx) + "->" + fa.getNameAsString();
    }

    if (e.isIntegerLiteralExpr()) return e.asIntegerLiteralExpr().getValue();
    if (e.isLongLiteralExpr()) return e.asLongLiteralExpr().getValue();
    if (e.isDoubleLiteralExpr()) return e.asDoubleLiteralExpr().getValue();
    if (e.isBooleanLiteralExpr()) return e.asBooleanLiteralExpr().getValue() ? "abap_true" : "abap_false";
    if (e.isStringLiteralExpr()) return "'" + e.asStringLiteralExpr().asString().replace("'", "''") + "'";
    if (e.isCharLiteralExpr()) return "'" + String.valueOf(e.asCharLiteralExpr().asChar()).replace("'", "''") + "'";

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
      if (op != null) return expr(b.getLeft(), ctx) + " " + op + " " + expr(b.getRight(), ctx);
    }

    if (e.isUnaryExpr()) {
      UnaryExpr u = e.asUnaryExpr();
      if (u.getOperator() == UnaryExpr.Operator.LOGICAL_COMPLEMENT) {
        return "NOT ( " + expr(u.getExpression(), ctx) + " )";
      }
      throw new UnsupportedExpression("Unary ++/-- als Expression nicht sicher in ABAP übersetzbar");
    }

    if (e.isConditionalExpr()) {
      ConditionalExpr c = e.asConditionalExpr();
      return "COND #( WHEN " + expr(c.getCondition(), ctx) +
          " THEN " + expr(c.getThenExpr(), ctx) +
          " ELSE " + expr(c.getElseExpr(), ctx) + " )";
    }

    if (e.isMethodCallExpr()) {
      MethodCallExpr mc = e.asMethodCallExpr();

      if (isSystemOutPrintln(mc) || isSystemOutPrint(mc)) {
        throw new UnsupportedExpression("System.out.* als Expression nicht sinnvoll");
      }

      // list.get(0) -> list[ 1 ]
      if (mc.getNameAsString().equals("get") && mc.getArguments().size() == 1 && mc.getScope().isPresent()) {
        Expression idx = mc.getArgument(0);
        String table = abapValue(mc.getScope().get(), ctx);
        if (idx.isIntegerLiteralExpr()) {
          int i0 = Integer.parseInt(idx.asIntegerLiteralExpr().getValue());
          return table + "[ " + (i0 + 1) + " ]";
        }
        return table + "[ " + expr(idx, ctx) + " + 1 ]";
      }

      throw new UnsupportedExpression("Method call als Expression nicht eindeutig (ABAP Parameternamen/Typen fehlen)");
    }

    if (e.isObjectCreationExpr()) {
      ObjectCreationExpr oce = e.asObjectCreationExpr();
      if (!oce.getArguments().isEmpty()) {
        throw new UnsupportedExpression("new ... mit Argumenten: ABAP braucht Parameternamen");
      }
      return "NEW " + mapJavaTypeToAbapClass(stripGeneric(oce.getTypeAsString())) + "( )";
    }

    throw new UnsupportedExpression("Expression-Typ nicht unterstützt: " + e.getClass().getSimpleName());
  }

  private String abapValue(Expression scope, Ctx ctx) {
    if (scope == null) return "me";
    if (scope.isThisExpr()) return "me";
    if (scope.isNameExpr()) return scope.asNameExpr().getNameAsString();
    if (scope.isFieldAccessExpr()) return accessExpr(scope.asFieldAccessExpr(), ctx);
    return expr(scope, ctx);
  }

  private String accessExpr(FieldAccessExpr fa, Ctx ctx) {
    Expression scope = fa.getScope();
    String name = fa.getNameAsString();

    if (scope.isThisExpr()) return "me->" + name;

    if (scope.isNameExpr()) {
      String left = scope.asNameExpr().getNameAsString();
      if (looksLikeTypeName(left)) return "zcl_" + toSnakeLower(left) + "=>" + name;
      return left + "->" + name;
    }

    if (scope.isFieldAccessExpr()) {
      return accessExpr(scope.asFieldAccessExpr(), ctx) + "->" + name;
    }

    return expr(scope, ctx) + "->" + name;
  }

  private String abapCallNoArgs(MethodCallExpr mc, Ctx ctx) {
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
      return accessExpr(scope.asFieldAccessExpr(), ctx) + "->" + name + "( ).";
    }

    return expr(scope, ctx) + "->" + name + "( ).";
  }

  // =========================================================
  // new -> ABAP NEW
  // =========================================================
  private void emitNewVar(String varName, String declaredType, ObjectCreationExpr oce, IndentWriter out, int indent, Ctx ctx) {
    String created = stripGeneric(oce.getTypeAsString());
    String abapClass = mapJavaTypeToAbapClass(created);

    out.line(indent, "DATA " + varName + " TYPE REF TO " + abapClass + ".");

    if (!oce.getArguments().isEmpty()) {
      emitTodoJava(out, indent, varName + " = " + oce + ";", "Konstruktor-Argumente: ABAP braucht Parameternamen");
      return;
    }

    out.line(indent, varName + " = NEW " + abapClass + "( ).");
  }

  private void emitNewAssign(String target, ObjectCreationExpr oce, IndentWriter out, int indent, Ctx ctx) {
    String created = stripGeneric(oce.getTypeAsString());
    String abapClass = mapJavaTypeToAbapClass(created);

    if (!oce.getArguments().isEmpty()) {
      emitTodoJava(out, indent, target + " = " + oce + ";", "Konstruktor-Argumente: ABAP braucht Parameternamen");
      return;
    }

    out.line(indent, target + " = NEW " + abapClass + "( ).");
  }

  // =========================================================
  // Enums
  // =========================================================
  private void indexEnums(CompilationUnit cu, Ctx ctx) {
    ctx.enumConstantsByType.clear();
    ctx.enumTypesByConstant.clear();

    for (EnumDeclaration ed : cu.findAll(EnumDeclaration.class)) {
      String type = ed.getNameAsString();
      List<String> constants = ed.getEntries().stream()
          .map(EnumConstantDeclaration::getNameAsString)
          .toList();
      ctx.enumConstantsByType.put(type, constants);
      for (String c : constants) {
        ctx.enumTypesByConstant.computeIfAbsent(c, k -> new ArrayList<>()).add(type);
      }
    }
  }

  private void inferEnumsFromSwitches(CompilationUnit cu, Ctx ctx) {
    for (SwitchStmt ss : cu.findAll(SwitchStmt.class)) {
      if (!ss.getSelector().isNameExpr()) continue;

      String varName = ss.getSelector().asNameExpr().getNameAsString();
      String typeName = resolveVarTypeName(ss, varName);
      if (typeName == null) continue;

      typeName = stripGeneric(typeName);
      if (!looksLikeTypeName(typeName)) continue;
      if (ctx.enumConstantsByType.containsKey(typeName)) continue;

      List<String> labels = ss.getEntries().stream()
          .flatMap(e -> e.getLabels().stream())
          .filter(Expression::isNameExpr)
          .map(e -> e.asNameExpr().getNameAsString())
          .distinct()
          .toList();

      if (labels.isEmpty()) continue;

      ctx.enumConstantsByType.put(typeName, labels);
      for (String c : labels) {
        ctx.enumTypesByConstant.computeIfAbsent(c, k -> new ArrayList<>()).add(typeName);
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

  private void emitEnumDeclarationsSnippet(IndentWriter out, Ctx ctx) {
    if (ctx.enumConstantsByType.isEmpty()) return;

    for (Map.Entry<String, List<String>> e : ctx.enumConstantsByType.entrySet()) {
      String enumType = e.getKey();
      String ty = "ty_" + toSnakeLower(enumType);

      out.line(0, "TYPES " + ty + " TYPE string.");
      for (String c : e.getValue()) {
        out.line(0, "CONSTANTS c_" + toSnakeLower(enumType) + "_" + toSnakeLower(c)
            + " TYPE " + ty + " VALUE '" + c + "'.");
      }
      out.blank();
    }
  }

  private void emitEnumDeclarationsInClassPublic(IndentWriter out, Ctx ctx, int indent) {
    if (ctx.enumConstantsByType.isEmpty()) return;

    out.line(indent, "\" --- enums (best-effort) ---");
    for (Map.Entry<String, List<String>> e : ctx.enumConstantsByType.entrySet()) {
      String enumType = e.getKey();
      String ty = "ty_" + toSnakeLower(enumType);
      out.line(indent, "TYPES " + ty + " TYPE string.");
      for (String c : e.getValue()) {
        out.line(indent, "CONSTANTS c_" + toSnakeLower(enumType) + "_" + toSnakeLower(c)
            + " TYPE " + ty + " VALUE '" + c + "'.");
      }
      out.blank();
    }
  }

  private Optional<String> resolveEnumConstantQualified(String enumType, String constant, Ctx ctx) {
    List<String> cs = ctx.enumConstantsByType.get(enumType);
    if (cs == null) return Optional.empty();
    if (!cs.contains(constant)) return Optional.empty();
    return Optional.of("c_" + toSnakeLower(enumType) + "_" + toSnakeLower(constant));
  }

  private Optional<String> resolveEnumConstantUnqualified(String constant, Ctx ctx) {
    List<String> types = ctx.enumTypesByConstant.getOrDefault(constant, List.of());
    if (types.size() != 1) return Optional.empty();
    return resolveEnumConstantQualified(types.get(0), constant, ctx);
  }

  // =========================================================
  // System.out helpers
  // =========================================================
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

  // =========================================================
  // Comments (Java -> ABAP)
  // =========================================================
  private void emitCompilationUnitHeaderComments(CompilationUnit cu, IndentWriter out) {
    if (cu == null) return;
    cu.getComment().ifPresent(c -> emitComment(c, out, 0));
  }

  private void emitLeadingComments(Node node, IndentWriter out, int indent) {
    if (node == null) return;
    node.getComment().ifPresent(c -> emitComment(c, out, indent));
  }

  private void emitComment(Comment c, IndentWriter out, int indent) {
    if (c == null) return;
    String raw = c.getContent();
    if (raw == null) raw = "";
    raw = raw.replace("\r\n", "\n").replace("\r", "\n").trim();

    if (c instanceof LineComment) {
      out.line(indent, "\" " + raw.trim());
      return;
    }

    if (c instanceof BlockComment || c instanceof JavadocComment) {
      for (String line : raw.split("\n")) {
        String t = line.strip();
        if (!t.isEmpty()) out.line(indent, "\" " + t);
      }
    }
  }

  // =========================================================
  // TODO output
  // =========================================================
  private void emitTodoJava(IndentWriter out, int indent, String javaCode, String reason) {
    String j = (javaCode == null ? "" : javaCode).replace("\r\n", "\n").replace("\r", "\n").trim();
    if (j.isEmpty()) {
      out.line(indent, "\" TODO: (leer) (weil: " + safe(reason) + ")");
      return;
    }

    String[] lines = j.split("\n");
    out.line(indent, "\" TODO: " + lines[0].trim() + (reason == null ? "" : "  (weil: " + safe(reason) + ")"));
    for (int i = 1; i < lines.length; i++) {
      String t = lines[i].trim();
      if (!t.isEmpty()) out.line(indent, "\"       " + t);
    }
  }

  private String safe(String s) {
    if (s == null) return "";
    return s.replace("\"", "'").trim();
  }

  // =========================================================
  // Type mapping
  // =========================================================
  private String mapTypeForAbap(String javaType, Ctx ctx) {
    String tRaw = javaType == null ? "" : javaType.trim();
    String t = stripGeneric(tRaw);

    if (tRaw.endsWith("[]")) {
      String elem = tRaw.substring(0, tRaw.length() - 2).trim();
      return "STANDARD TABLE OF " + mapTypeForAbap(elem, ctx) + " WITH EMPTY KEY";
    }

    if (ctx.enumConstantsByType.containsKey(t)) {
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

  // =========================================================
  // Parse robustness
  // =========================================================
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

  // =========================================================
  // Mixed extraction for snippet
  // =========================================================
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

  // =========================================================
  // Java main detection
  // =========================================================
  private Optional<MethodDeclaration> findJavaMain(ClassOrInterfaceDeclaration cls) {
    for (MethodDeclaration md : cls.getMethods()) {
      if (!md.getNameAsString().equals("main")) continue;
      if (!md.isStatic()) continue;
      if (!md.getType().isVoidType()) continue;
      if (md.getParameters().size() != 1) continue;

      Parameter p = md.getParameter(0);
      String t = p.getTypeAsString().replace(" ", "");
      // String[] args OR String... args
      if (t.equals("String[]") || t.equals("String...")) return Optional.of(md);
    }
    return Optional.empty();
  }

  // =========================================================
  // Helpers
  // =========================================================
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

  // =========================================================
  // Output helper
  // =========================================================
  private static final class IndentWriter {
    private final StringBuilder sb = new StringBuilder();

    void line(int indent, String line) {
      sb.append(" ".repeat(Math.max(0, indent))).append(line).append("\n");
    }

    void blank() { sb.append("\n"); }

    String finish() {
      String s = sb.toString().replace("\r\n", "\n").replace("\r", "\n").trim();
      return s.isEmpty() ? "\n" : (s + "\n");
    }
  }
}
