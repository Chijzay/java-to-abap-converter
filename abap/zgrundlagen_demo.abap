REPORT zgrundlagen_demo.

" 1. Variablen und einfache Typen
DATA: lv_zahl      TYPE i VALUE 5,
      lv_preis     TYPE p DECIMALS 2 VALUE '19.99',
      lv_aktiv     TYPE abap_bool VALUE abap_true,
      lv_buchstabe TYPE c LENGTH 1 VALUE 'A',
      lv_name      TYPE string VALUE 'Max',
      lv_eingabe   TYPE i,
      lv_summe     TYPE i.

" 2. Einfache Ausgabe
WRITE: / 'Willkommen zur ABAP-Grundlagendemo!'.
WRITE: / 'Name:', lv_name, ', Preis:', lv_preis.

" 3. Benutzereingabe (Parameter-Screen)
PARAMETERS: p_eingabe TYPE i.

lv_eingabe = p_eingabe.

" 4. IF/ELSE
IF lv_eingabe > 10.
  WRITE: / 'Die Zahl ist größer als 10.'.
ELSE.
  WRITE: / 'Die Zahl ist kleiner oder gleich 10.'.
ENDIF.

" 5. Schleife und Array (interne Tabelle)
DATA: lt_zahlen TYPE STANDARD TABLE OF i WITH EMPTY KEY,
      lv_z TYPE i.

APPEND 1 TO lt_zahlen.
APPEND 2 TO lt_zahlen.
APPEND 3 TO lt_zahlen.
APPEND 4 TO lt_zahlen.
APPEND 5 TO lt_zahlen.

WRITE: / 'Array: '.
LOOP AT lt_zahlen INTO lv_z.
  WRITE: lv_z NO-GAP.
ENDLOOP.

" 6. CASE/SWITCH
CASE lv_eingabe.
  WHEN 1.
    WRITE: / 'Du hast Eins eingegeben.'.
  WHEN 2.
    WRITE: / 'Du hast Zwei eingegeben.'.
  WHEN OTHERS.
    WRITE: / 'Eine andere Zahl wurde eingegeben.'.
ENDCASE.

" 7. Methodenaufruf (Summe berechnen)
lv_summe = addiere( lv_zahl, lv_eingabe ).
WRITE: / 'Summe von', lv_zahl, '+', lv_eingabe, '=', lv_summe.

" 8. Objektorientierung: Klasse & Methode
START-OF-SELECTION.
  DATA(lo_person) = NEW lcl_person( name = lv_name alter = 25 ).
  lo_person->begruesse( ).

" 9. Klasse mit Konstruktor und Methode
CLASS lcl_person DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING name TYPE string alter TYPE i,
      begruesse.
  PRIVATE SECTION.
    DATA: mv_name  TYPE string,
          mv_alter TYPE i.
ENDCLASS.

CLASS lcl_person IMPLEMENTATION.
  METHOD constructor.
    mv_name = name.
    mv_alter = alter.
  ENDMETHOD.

  METHOD begruesse.
    WRITE: / 'Hallo, ich bin', mv_name, 'und', mv_alter, 'Jahre alt.'.
  ENDMETHOD.
ENDCLASS.

" 10. Funktion für Summe (ähnlich wie Methode in Java)
FORM addiere USING iv_a TYPE i iv_b TYPE i RETURNING VALUE(rv_summe) TYPE i.
  rv_summe = iv_a + iv_b.
ENDFORM.
