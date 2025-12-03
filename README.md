# Der kompakte Vergleich zwischen Java und ABAP

_Einstieg in ABAP für Java-Entwickler_

Kurzreferenz mit Parallelbeispielen in Java und ABAP, als Einstiegshilfe für Java-Entwickler beim ersten Kontakt mit ABAP (SAP-Berufseinstieg), mit Fokus auf Basics, typische SAP-Patterns und kurze, verständliche Beispiele.

## Projektstruktur

```
.
├─ java/
│  └─ GrundlagenDemo.java                   # Beispiele in Java
├─ abap/
│  └─ zgrundlagen_demo.abap                 # Beispiele in ABAP
├─ Einstieg_in_ABAP_fuer_Java-Entwickler    # Cheatsheet
└─ README.md
```

## Java Beispiel ausführen

Voraussetzung: JDK 17+ (geht meistens auch mit älteren Versionen, je nach Syntax).

```
cd java
javac GrundlagenDemo.java
java GrundlagenDemo
```

## ABAP Beispiel ausführen

ABAP ist in der Regel nur innerhalb einer SAP-Umgebung lauffähig (SAP GUI / ADT in Eclipse und ABAP-System).

- Öffne z. B. ADT (Eclipse) oder SE38/SE80 im SAP-System.
- Erstelle einen ausführbaren Report, z. B. `ZGRUNDLAGEN_DEMO`.
- Kopiere den Inhalt aus `abap/zgrundlagen_demo.abap` hinein.
- Aktivieren und ausführen.

**Hinweis:** Das `Z` am Anfang ist der übliche Kunden-Namensraum in SAP. Wenn dein System andere Regeln bzw. Namespaces nutzt, passe den Namen entsprechend an.

## Was du hier lernst (Kurzüberblick)

- Java `List`, `Array` vs. ABAP interne Tabellen
- Java `if`, `for`, `switch` vs. ABAP `IF`, `LOOP`, `CASE`
- Java Methoden und Classes vs. ABAP Methoden und Klassen (ABAP Objects)
- SQL in Java JDBC und ORM vs. ABAP Open SQL
- Typische Performance-Fallen (z. B. `SELECT` in Loops)
