       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC219A-SUB1.
      *
      * Subprogram 1 for IC219A: string operation.
      * Sets the alpha parameter to "HELLO".
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-ALPHA PIC X(10).
       PROCEDURE DIVISION USING LS-ALPHA.
           MOVE "HELLO     " TO LS-ALPHA.
           GOBACK.
