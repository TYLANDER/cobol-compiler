       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC208A-SUB.
      *
      * Subprogram for IC208A: BY REFERENCE multi-modify test.
      * Modifies all three BY REFERENCE parameters to verify
      * the caller sees each change.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-NAME   PIC X(10).
       01 LS-CODE   PIC 9(4).
       01 LS-STATUS PIC X(8).
       PROCEDURE DIVISION USING LS-NAME LS-CODE LS-STATUS.
           MOVE "AFTER     " TO LS-NAME.
           MOVE 4444 TO LS-CODE.
           MOVE "COMPLETE" TO LS-STATUS.
