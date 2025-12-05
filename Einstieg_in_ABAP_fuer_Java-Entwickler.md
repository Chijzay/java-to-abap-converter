# Einführung in ABAP für Java-Entwickler


## ABAP Überblock und seine typischen Einsatzgebiete

ABAP (Advanced Business Application Programming) ist die herstellerspezifische Programmiersprache der
Firma SAP und die Programmiersprache für SAP-Geschäftsanwendungen – von klassischem ERP bis S/4HANA.. Sie wurde ursprünglich in den 1980er Jahren als „Allgemeiner Berichtsaufbereitungsprozessor“ entwickelt, um Berichte im SAP-System zu erstellen . Mit der Einführung von SAP R/3 in den 1990er Jahren wurde ABAP deutlich erweitert. Seither basiert das gesamte SAP-ERP-System auf ABAP. Unternehmen nutzen ABAP, um ihre SAP-Lösungen an individuelle Geschäftsanforderungen (Customizing) anzupassen. Ein Java-Entwickler stößt früher oder später auf ABAP. Diese Dokumentation führt in ABAP ein, speziell aus Sicht eines Java-Kenners. Jeder Abschnitt vergleicht ABAP mit Java, um beide Programmiersprachen verständlich zu veranschaulichen.

### Typische Einsatzgebiete von ABAP sind unter anderem:

- Reports (Berichte erstellen oder Auswertungen): Erstellung von Auswertungsprogrammen, die Daten aus der SAP-Datenbank lesen und in übersichtlicher Form darstellen (z.B. Listenberichte, ALVReports).

- Schnittstellen: Entwicklung von Programmen, die Daten mit Drittsystemen austauschen (z.B. über IDocs, RFC/Webservices), um SAP in die bestehende IT-Landschaft zu integrieren.

- Datenmigration: Massenhafte Übernahme oder Bereinigung von Daten, z.B. beim Einspielen von Altdaten in ein neues SAP-System oder beim Transfer von Daten zwischen Systemen. Hierfür werden häufig spezielle ABAP-Programme oder Tools (wie LSMW) eingesetzt.

- Formulare und Druckprogramme: Erstellung von Druckformularen (z.B. Rechnungen, Lieferscheine) mittels Technologien wie Smart Forms oder Adobe Forms, die im Kern ebenfalls ABAP-basiert sind.

- Erweiterungen und Automatisierungen: Implementierung kundenspezifischer Geschäftslogik, Anpassung von SAP-Standardfunktionen (User-Exits, BAdIs) sowie Automatisierung von Geschäftsprozessen durch eigene ABAP-Codebausteine.

Kurz gesagt: ABAP ist die wesentloche Sprache im SAP-Ökosystem, um maßgeschneiderte Geschäftslogik und Datenverarbeitung direkt auf der SAP-Plattform umzusetzen. Im SAP-Kontext übernimmt ABAP Aufgaben, die man in anderen Umgebungen z.B. mit Java, Python oder SQL lösen würde – allerdings optimiert innerhalb des SAP-Systems. So sorgt ABAP für effiziente Datenbankzugriffe und einen reibungslosen Datenaustausch im SAP-Kern.


## Grundlagen der ABAP-Syntax im Vergleich zu Java

Die ABAP-Syntax unterscheidet sich in mehreren Punkten von Java, obwohl beide prozedurale bzw. objektorientierte Sprachen sind. Als Java-Entwickler sollte man insbesondere auf folgende Unterschiede achten:

- **Schlüsselwörter statt Klammern:** ABAP verwendet Schlüsselwort-Paare zur Strukturierung anstelle von geschweiften Klammern. Codeblöcke werden z.B. mit `IF ... ENDIF`, `DO ... ENDDO` oder `LOOP ... ENDLOOP` eingerahmt, anstatt `{ ... }` zu verwenden.
  
- **Anweisungsende mit Punkt:** Jede ABAP-Anweisung wird mit einem Punkt `.` abgeschlossen, nicht mit Semikolon. Ein Punkt in ABAP entspricht in Java einem `;` am Zeilenende.
- **Groß- und Kleinschreibung:** ABAP ist nicht case-sensitiv. Schlüsselwörter und Bezeichner können in Groß- oder Kleinschreibung geschrieben werden. Traditionell wird ABAP-Code oft komplett in Großbuchstaben verfasst, was aber keine syntaktische Pflicht ist.
  
- **Selbstreferenz und Methodenaufruf:** In ABAP-Objektmethoden referenziert man das aktuelle Objekt mit `me` (statt `this` in Java). Methoden werden mit `->` aufgerufen (für Instanzmethoden) bzw. `=>` (für Klassenmethoden), im Gegensatz zum Punktnotation `.` in Java. Beispiel: `obj->methode( ).` entspricht in etwa `obj.methode();` in Java.
- **Objekt-Instanziierung:** Anstatt des `new`-Operators wie in Java nutzt ABAP den Befehl `CREATE OBJECT` zur Instanziierung von Objekten. Beispiel: `CREATE OBJECT lo_obj.` (vorher muss `lo_obj` als Objekt-Referenz deklariert sein).
  
- **Variablendeklaration und -typisierung:** ABAP erfordert die Deklaration von Variablen mit expliziten Typen (stark typisiert). Üblicherweise werden alle `DATA`-Variablen und `TYPES`Definitionen am Anfang eines Programms oder einer Prozedur deklariert. Innerhalb von Methoden ist zwar seit neueren ABAP-Versionen auch eine Deklaration im Codeblock möglich (inline mit `DATA(var) = ...`), doch die klassische Konvention ist, erst die Datenvariablen zu definieren und dann die Logik zu schreiben.
  
- **Reservierte Wörter:** ABAP hat einen sehr umfangreichen Satz an Schlüsselwörtern (über 700) im Vergleich zu Java (rund 50). Viele Sprachkonstrukte sind in ABAP bereits durch schlüsselwortartige Befehle abgedeckt (z.B. `MOVE` statt Zuweisung, `LOOP AT ...` statt `for`-Schleife). Dies macht den Code teilweise selbstdokumentierender, aber auch umfangreicher. Die ABAP-Entwicklungsumgebung bietet eine kontextsensitive F1-Hilfe für jedes Schlüsselwort.

Ein einfaches ABAP-Programm ähnelt strukturell einem kleinen Skript, das vom SAP-Laufzeitsystem ereignisgesteuert ausgeführt wird. Es gibt keine `main()`-Methode wie in Java. Stattdessen beginnen ABAP-Programme sofort mit ausführbaren Anweisungen oder definieren verarbeitungsblöcke (z.B. Events wie `START-OF-SELECTION` für Reports). Beim Ausführen eines Reports durchläuft das System vordefinierte Ereignisse in einer festen Reihenfolge (Initialisierung, Selektionsbildschirm, Start-of-Selection, etc.), an die man Code anhängen kann. Dieses Konzept wird im Abschnitt über Reporte noch näher erläutert. Insgesamt sollte die ABAP-Syntax aber keinen Java-Entwickler abschrecken. Sie ist nur etwas anders und stark an die Bedürfnisse der Geschäftsprogrammierung angepasst (z.B. eingebettete SQL-Queries, Formatierungsausgaben, etc.)







| Aspekt               |  ABAP-Beispiel                     | Java-Beispiel                           |
|----------------------|------------------------------------|-----------------------------------------|
| Anweisungsende       | `ADD 1 TO lv_count.`               | `count = count + 1;`                    |
| Codeblock            | `IF bedingung. ... ENDIF.`         | `if(condition) { ... }`                 |
| Kommentar            | `WRITE 'Hi'. " Ausgabe`            | `System.out.println("Hi"); // Ausgabe`  |
| Referenz auf sich    | `me` _(innerhalb einer Methode)_   | `this` _(innerhalb einer Methode)_      |
| Objekt instanzieren  | `DATA lo_obj TYPE REF TO zcl_meine_klasse.`            | `MeineKlasse obj = new MeineKlasse();`              |
|                      | `CREATE OBJECT lo_obj.`            |                                         |  
| Methodenaufruf       | `lo_obj->methode( ).`              | `obj.methode();`                        |

