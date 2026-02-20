       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC219A-SUB2.
      *
      * Subprogram 2 for IC219A: numeric computation.
      * Multiplies the parameter by 100.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-NUM PIC 9(4).
       PROCEDURE DIVISION USING LS-NUM.
           MULTIPLY LS-NUM BY 100 GIVING LS-NUM.
           GOBACK.
