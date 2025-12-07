package de.example.j2abap;

import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;

import java.util.ArrayList;
import java.util.List;

public final class MethodSignatureBuilder {
  private final TypeMapper typeMapper;

  public MethodSignatureBuilder(TypeMapper typeMapper) {
    this.typeMapper = typeMapper;
  }

  public String buildMethodsLine(MethodDeclaration m) {
    StringBuilder sb = new StringBuilder();
    sb.append("METHODS ").append(m.getNameAsString());

    List<Parameter> params = m.getParameters();
    if (!params.isEmpty()) {
      sb.append(" IMPORTING ");
      List<String> parts = new ArrayList<>();
      for (Parameter p : params) {
        String abapType = typeMapper.mapToAbap(p.getType());
        parts.add(p.getNameAsString() + " TYPE " + abapType);
      }
      sb.append(String.join(" ", parts));
    }

    if (!typeMapper.isVoid(m.getType())) {
      String retType = typeMapper.mapToAbap(m.getType());
      sb.append(" RETURNING VALUE(rv_result) TYPE ").append(retType);
    }

    sb.append(".");
    return sb.toString();
  }
}
