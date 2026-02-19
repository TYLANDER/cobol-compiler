       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC203A-SUB.
      *
      * Subprogram for IC203A: multiple parameter types.
      * Receives one alphanumeric and two numeric fields,
      * sets each to specific values.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-NAME   PIC X(10).
       01 LS-AGE    PIC 9(3).
       01 LS-SALARY PIC 9(6).
       PROCEDURE DIVISION USING LS-NAME LS-AGE LS-SALARY.
           MOVE "JOHN      " TO LS-NAME.
           MOVE 30 TO LS-AGE.
           MOVE 50000 TO LS-SALARY.
