CLASS zcl_local_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_local_test IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    DATA o TYPE REF TO object.
    DATA s TYPE string.

    CREATE OBJECT o TYPE ('\PROGRAM=ZBP_I_RAP_TRAVEL_MIPO=========CP\CLASS=LHC_TRAVEL').

    CALL METHOD o->('FOO') RECEIVING result = s.

    out->write( s ).

  ENDMETHOD.

ENDCLASS.
