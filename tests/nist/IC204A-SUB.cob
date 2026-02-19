       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC204A-SUB.
      *
      * Subprogram for IC204A: output parameter test.
      * Receives a numeric input, doubles it, and stores
      * the result in the output parameter.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-INPUT  PIC 9(4).
       01 LS-RESULT PIC 9(4).
       PROCEDURE DIVISION USING LS-INPUT LS-RESULT.
           MULTIPLY LS-INPUT BY 2 GIVING LS-RESULT.
