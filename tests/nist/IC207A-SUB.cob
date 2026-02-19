       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC207A-SUB.
      *
      * Subprogram for IC207A: mixed-type parameter test.
      * Receives four parameters of mixed types (alpha, numeric)
      * and sets each to specific values.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-ALPHA  PIC X(10).
       01 LS-NUM    PIC 9(6).
       01 LS-CODE   PIC X(4).
       01 LS-AMT    PIC 9(4).
       PROCEDURE DIVISION USING LS-ALPHA LS-NUM LS-CODE LS-AMT.
           MOVE "MIXED-OK  " TO LS-ALPHA.
           MOVE 123456 TO LS-NUM.
           MOVE "GRPX" TO LS-CODE.
           MOVE 9876 TO LS-AMT.
