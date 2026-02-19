       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC105A-SUB.
      *
      * Subprogram for IC105A: string processing.
      * Receives an alphanumeric field and sets it to
      * "HELLO BACK".
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-MSG PIC X(10).
       PROCEDURE DIVISION USING LS-MSG.
           MOVE "HELLO BACK" TO LS-MSG.
