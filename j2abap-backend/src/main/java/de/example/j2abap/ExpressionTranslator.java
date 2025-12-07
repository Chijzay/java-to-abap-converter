package de.example.j2abap;

import com.github.javaparser.ast.expr.*;
import de.example.j2abap.util.Strings;

import java.util.stream.Collectors;

public final class ExpressionTranslator {
  private final TypeMapper typeMapper;

  public ExpressionTranslator(TypeMapper typeMapper) {
    this.typeMapper = typeMapper;
  }

  public String toAbap(Expression e) {
    if (e == null) return "''";

    if (e.isNameExpr()) {
      return e.asNameExpr().getNameAsString();
    }

    if (e.isStringLiteralExpr()) {
      return "'" + Strings.escapeAbapString(e.asStringLiteralExpr().asString()) + "'";
    }

    if (e.isCharLiteralExpr()) {
      char c = e.asCharLiteralExpr().asChar();
      return "'" + Strings.escapeAbapString(String.valueOf(c)) + "'";
    }

    if (e.isIntegerLiteralExpr()) {
      return e.asIntegerLiteralExpr().getValue();
    }

    if (e.isLongLiteralExpr()) {
      return e.asLongLiteralExpr().getValue().replace("L", "").replace("l", "");
    }

    if (e.isDoubleLiteralExpr()) {
      return e.asDoubleLiteralExpr().getValue();
    }

    if (e.isBooleanLiteralExpr()) {
      return e.asBooleanLiteralExpr().getValue() ? "abap_true" : "abap_false";
    }

    if (e.isEnclosedExpr()) {
      return "(" + toAbap(e.asEnclosedExpr().getInner()) + ")";
    }

    if (e.isBinaryExpr()) {
      BinaryExpr b = e.asBinaryExpr();
      String op = mapBinaryOp(b.getOperator());
      return toAbap(b.getLeft()) + " " + op + " " + toAbap(b.getRight());
    }

    if (e.isUnaryExpr()) {
      UnaryExpr u = e.asUnaryExpr();
      return switch (u.getOperator()) {
        case LOGICAL_COMPLEMENT -> "NOT " + toAbap(u.getExpression());
        case MINUS -> "-" + toAbap(u.getExpression());
        case PLUS -> "+" + toAbap(u.getExpression());
        default -> "/* TODO unary " + u.getOperator() + " */ " + toAbap(u.getExpression());
      };
    }

    if (e.isAssignExpr()) {
      AssignExpr a = e.asAssignExpr();
      if (a.getOperator() == AssignExpr.Operator.ASSIGN) {
        return toAbap(a.getTarget()) + " = " + toAbap(a.getValue());
      }
      return "/* TODO assign-op " + a.getOperator() + " */ " + toAbap(a.getTarget()) + " = " + toAbap(a.getValue());
    }

    if (e.isFieldAccessExpr()) {
      FieldAccessExpr f = e.asFieldAccessExpr();
      return toAbap(f.getScope()) + "-" + f.getNameAsString();
    }

    if (e.isMethodCallExpr()) {
      MethodCallExpr m = e.asMethodCallExpr();
      String args = m.getArguments().stream().map(this::toAbap).collect(Collectors.joining(", "));
      String name = m.getNameAsString();

      if (m.getScope().isPresent()) {
        String scope = toAbap(m.getScope().get());
        return "/* TODO call */ " + scope + "->" + name + "( " + args + " )";
      }
      return "/* TODO call */ " + name + "( " + args + " )";
    }

    return "/* TODO expr: " + e.getClass().getSimpleName() + " */";
  }

  private String mapBinaryOp(BinaryExpr.Operator op) {
    return switch (op) {
      case PLUS -> "+";
      case MINUS -> "-";
      case MULTIPLY -> "*";
      case DIVIDE -> "/";
      case REMAINDER -> "MOD";
      case EQUALS -> "=";
      case NOT_EQUALS -> "<>";
      case LESS -> "<";
      case LESS_EQUALS -> "<=";
      case GREATER -> ">";
      case GREATER_EQUALS -> ">=";
      case AND -> "AND";
      case OR -> "OR";
      default -> "/* TODO op " + op + " */";
    };
  }
}
