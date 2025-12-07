package de.example.j2abap;

import com.github.javaparser.ast.type.PrimitiveType;
import com.github.javaparser.ast.type.Type;

import java.util.Locale;

public final class TypeMapper {

  public String mapToAbap(Type t) {
    if (t == null) return "string";

    if (t.isPrimitiveType()) {
      PrimitiveType pt = t.asPrimitiveType();
      return switch (pt.getType()) {
        case INT, SHORT, BYTE -> "i";
        case LONG -> "int8";
        case BOOLEAN -> "abap_bool";
        case DOUBLE, FLOAT -> "f";
        case CHAR -> "c LENGTH 1";
      };
    }

    String name = t.asString();
    String lower = name.toLowerCase(Locale.ROOT);

    if (lower.equals("string")) return "string";
    if (lower.equals("integer")) return "i";
    if (lower.equals("long")) return "int8";
    if (lower.equals("boolean")) return "abap_bool";
    if (lower.equals("double") || lower.equals("float")) return "f";

    return "REF TO object";
  }

  public boolean isVoid(Type t) {
    return t != null && t.isVoidType();
  }
}
