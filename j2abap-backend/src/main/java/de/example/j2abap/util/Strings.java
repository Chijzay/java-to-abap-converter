package de.example.j2abap.util;

public final class Strings {
  private Strings() {}

  public static String escapeAbapString(String s) {
    return s.replace("'", "''");
  }
}
