package de.example.j2abap;

import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.stmt.*;
import com.github.javaparser.ast.type.Type;

import java.util.List;

public final class StatementTranslator {
  private final ExpressionTranslator expr;
  private final TypeMapper typeMapper;

  public StatementTranslator(ExpressionTranslator expr, TypeMapper typeMapper) {
    this.expr = expr;
    this.typeMapper = typeMapper;
  }

  /** Für Snippets (Statements ohne Methodensignatur). */
  public void emitStatements(AbapEmitter out, List<Statement> statements) {
    boolean hasReturnValue = false;
    for (Statement s : statements) {
      emitStatement(out, s, hasReturnValue);
    }
  }

  public void emitMethodBody(AbapEmitter out, MethodDeclaration m) {
    if (m.getBody().isEmpty()) {
      out.line("\" TODO: no body (abstract/interface/native?)");
      return;
    }

    Type retType = m.getType();
    boolean hasReturnValue = !typeMapper.isVoid(retType);

    for (Statement s : m.getBody().get().getStatements()) {
      emitStatement(out, s, hasReturnValue);
    }
  }

  private void emitStatement(AbapEmitter out, Statement s, boolean hasReturnValue) {
    if (s.isBlockStmt()) {
      for (Statement inner : s.asBlockStmt().getStatements()) {
        emitStatement(out, inner, hasReturnValue);
      }
      return;
    }

    if (s.isIfStmt()) {
      emitIf(out, s.asIfStmt(), hasReturnValue);
      return;
    }

    if (s.isReturnStmt()) {
      emitReturn(out, s.asReturnStmt(), hasReturnValue);
      return;
    }

    if (s.isExpressionStmt()) {
      emitExpressionStmt(out, s.asExpressionStmt(), hasReturnValue);
      return;
    }

    if (s.isEmptyStmt()) return;

    out.line("\" TODO stmt: " + s.getClass().getSimpleName());
  }

  private void emitIf(AbapEmitter out, IfStmt ifs, boolean hasReturnValue) {
    String cond = expr.toAbap(ifs.getCondition());
    out.line("IF " + cond + ".");
    out.indent(() -> emitStatement(out, ifs.getThenStmt(), hasReturnValue));

    if (ifs.getElseStmt().isPresent()) {
      out.line("ELSE.");
      out.indent(() -> emitStatement(out, ifs.getElseStmt().get(), hasReturnValue));
    }
    out.line("ENDIF.");
  }

  private void emitReturn(AbapEmitter out, ReturnStmt r, boolean hasReturnValue) {
    if (r.getExpression().isPresent()) {
      String value = expr.toAbap(r.getExpression().get());
      if (hasReturnValue) {
        out.line("rv_result = " + value + ".");
        out.line("RETURN.");
      } else {
        out.line("\" TODO: return value in snippet/void context: " + value);
        out.line("RETURN.");
      }
    } else {
      out.line("RETURN.");
    }
  }

  private void emitExpressionStmt(AbapEmitter out, ExpressionStmt es, boolean hasReturnValue) {
    Expression e = es.getExpression();

    if (e.isVariableDeclarationExpr()) {
      emitVarDecl(out, e.asVariableDeclarationExpr());
      return;
    }

    if (e.isMethodCallExpr() && isSystemOutPrintln(e.asMethodCallExpr())) {
      MethodCallExpr mc = e.asMethodCallExpr();
      if (mc.getArguments().isEmpty()) {
        out.line("WRITE: / ''.");
      } else {
        out.line("WRITE: / " + expr.toAbap(mc.getArgument(0)) + ".");
      }
      return;
    }

    if (e.isAssignExpr()) {
      out.line(expr.toAbap(e) + ".");
      return;
    }

    out.line(expr.toAbap(e) + ".");
  }

  private void emitVarDecl(AbapEmitter out, VariableDeclarationExpr vde) {
    vde.getVariables().forEach(v -> {
      String name = v.getNameAsString();
      String abapType = typeMapper.mapToAbap(v.getType());

      out.line("DATA " + name + " TYPE " + abapType + ".");
      if (v.getInitializer().isPresent()) {
        out.line(name + " = " + expr.toAbap(v.getInitializer().get()) + ".");
      }
    });
  }

  private boolean isSystemOutPrintln(MethodCallExpr m) {
    if (!m.getNameAsString().equals("println")) return false;
    if (m.getScope().isEmpty()) return false;

    Expression scope = m.getScope().get();
    if (scope.isFieldAccessExpr()) {
      FieldAccessExpr fa = scope.asFieldAccessExpr();
      if (!fa.getNameAsString().equals("out")) return false;
      Expression faScope = fa.getScope();
      return faScope.isNameExpr() && faScope.asNameExpr().getNameAsString().equals("System");
    }
    return false;
  }
}
