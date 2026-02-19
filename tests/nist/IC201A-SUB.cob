       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC201A-SUB.
      *
      * Subprogram for IC201A: BY REFERENCE parameter test.
      * Modifies both parameters to verify BY REFERENCE passing.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-VALUE PIC X(10).
       01 LS-NUM   PIC 9(4).
       PROCEDURE DIVISION USING LS-VALUE LS-NUM.
           MOVE "MODIFIED  " TO LS-VALUE.
           MOVE 1234 TO LS-NUM.
