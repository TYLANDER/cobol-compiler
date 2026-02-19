       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC103A-SUB.
      *
      * Subprogram for IC103A: GOBACK test.
      * Sets a value and returns via GOBACK.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-STATUS PIC 9(4).
       PROCEDURE DIVISION USING LS-STATUS.
           MOVE 77 TO LS-STATUS.
           GOBACK.
