package de.example.j2abap;

public final class AbapEmitter {
  private final StringBuilder sb = new StringBuilder();
  private int indent = 0;

  public void indent(Runnable r) {
    indent++;
    try { r.run(); }
    finally { indent--; }
  }

  public void line(String s) {
    sb.append("  ".repeat(Math.max(0, indent))).append(s).append("\n");
  }

  public void blank() {
    sb.append("\n");
  }

  @Override
  public String toString() {
    return sb.toString();
  }
}
