---
title: "Mein Bericht"
output:
  word_document:
    toc: true
    toc_depth: 3
---

# abap-vs-java-basics

Parallel-Beispiele in **Java** und **ABAP**, um den Einstieg in ABAP aus Java-Sicht zu erleichtern. Fokus: Basics, typische Patterns im SAP-Umfeld und kurze, verständliche Beispiele.

## Inhalte / Lernpfad
1. ABAP-Syntax & Datentypen  
2. Interne Tabellen + LOOP/READ/SORT  
3. Open SQL (SELECT, JOIN, WHERE, INTO TABLE)  
4. Debugging & typische Performance-Fallen  
5. OOP-Grundlagen + saubere Methoden  
6. ALV-Ausgabe / einfache Reports  
7. Ausblick: BAPIs / IDocs / OData / CDS

## Projektstruktur (Vorschlag)

```
.
├─ java/
│  └─ GrundlagenDemo.java
├─ abap/
│  └─ zgrundlagen_demo.abap
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
