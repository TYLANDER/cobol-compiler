       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC102A-SUB.
      *
      * Subprogram for IC102A: multiple parameters.
      * Receives alpha and numeric, modifies both.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-NAME PIC X(10).
       01 LS-CODE PIC 9(4).
       PROCEDURE DIVISION USING LS-NAME LS-CODE.
           MOVE "RETURNED  " TO LS-NAME.
           MOVE 99 TO LS-CODE.
