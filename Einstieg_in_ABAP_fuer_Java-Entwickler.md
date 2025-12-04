# Einstieg in ABAP für Java-Entwickler

...




| Aspekt               |  ABAP-Beispiel                     | Java-Beispiel                           |
|----------------------|------------------------------------|-----------------------------------------|
| Anweisungsende       | `ADD 1 TO lv_count.`               | `count = count + 1;`                    |
| Codeblock            | `IF bedingung. ... ENDIF.`         | `if(condition) { ... }`                 |
| Kommentar            | `WRITE 'Hi'. " Ausgabe`            | `System.out.println("Hi"); // Ausgabe`  |
| Referenz auf sich    | `me` _(innerhalb einer Methode)_   | `this` _(innerhalb einer Methode)_      |
| Objekt instanzieren  | `DATA lo_obj TYPE REF TO zcl_meine_klasse.`            | `MeineKlasse obj = new MeineKlasse();`              |
|                      | `CREATE OBJECT lo_obj.`            |                                         |
| Methodenaufruf       | `lo_obj->methode( ).`              | `obj.methode();`                        |
|----------------------|------------------------------------|-----------------------------------------|


CREATE OBJECT
lo_obj. <br> " (Vorher: DATA
lo_obj TYPE REF TO Class.)
