## Was dieses Java-Programm zeigt:

| Konzept                          | Beispiel im Code                                                                     |
| -------------------------------- | ------------------------------------------------------------------------------------ |
| Einstiegspunkt                   | `public static void main(String[] args)`                                             |
| Importe                          | `import java.util.Scanner;`, `import java.util.List;`, `import java.util.ArrayList;` |
| Ausgaben (Konsole)               | `System.out.println(...)`, `System.out.print(...)`                                   |
| Datentypen (primitiv und Referenz) | `int`, `double`, `boolean`, `char`, `String`                                       |
| Variablen                        | `int zahl = 5;`, `String name = "unbekannt";`                                        |
| Konstanten (`final`)             | `private static final int PERSON_STANDARDALTER = 25;`                                |
| `static` vs. Instanz             | `berechneSumme(...)` *(static)* vs. `person.begruesse()` *(Instanz)*                 |
| Benutzer-Eingabe                 | `try (Scanner scanner = new Scanner(System.in)) { ... }`                             |
| Fehlerbehandlung (`try/catch`)   | `catch (Exception e) { ... }`                                                        |
| Bedingungen                      | `if (...) { ... } else { ... }`                                                      |
| Switch-Auswahl                   | `switch (eingabe) { case 1 -> ... }`                                                 |
| Ternary Operator                 | `String aktivText = istAktiv ? "aktiv" : "inaktiv";`                                 |
| Arrays (feste Größe)             | `int[] zahlen = {1, 2, 3, 4, 5};`                                                    |
| Schleifen (for-each)             | `for (int z : zahlen) { ... }`                                                       |
| Schleifen (while)                | `while (i < 3) { ... }`                                                              |
| Collections/Listen               | `List<String> namen = new ArrayList<>();`, `namen.add(...)`                          |
| Methoden (Parameter und Return)  | `public static int berechneSumme(int a, int b) { return a + b; }`                    |
| Klassen/Objekte (OOP)            | `class Person`, `Person person = new Person(...);`                                   |
| Konstruktor                      | `public Person(String name, int alter) { ... }`                                      |
| Kapselung (private Felder)       | `private String name;`, `private int alter;`                                         |
| Getter und Setter                | `getName()`, `setName(...)`                                                          |
| Objekt-Zustand ändern            | `person.hatGeburtstag();` *(alter++)*                                                |
| `toString()` und `@Override`     | `@Override public String toString() { ... }` und `System.out.println(person);`       |
| String-Vergleich korrekt         | `"unbekannt".equals(name)` *(statt `==`)*                                            |
| `null`-Grundlage                 | `Person maybeNull = null; if (maybeNull == null) { ... }`                            |
| Ressourcen sauber schließen      | `try-with-resources` schließt `Scanner` automatisch                                  |
