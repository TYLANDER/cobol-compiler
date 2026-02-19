       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC101A-SUB.
      *
      * Subprogram for IC101A: basic CALL test.
      * Receives a numeric field and sets it to 42.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-RESULT PIC 9(4).
       PROCEDURE DIVISION USING LS-RESULT.
           MOVE 42 TO LS-RESULT.
