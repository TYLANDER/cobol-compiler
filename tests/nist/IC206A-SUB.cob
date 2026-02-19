       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC206A-SUB.
      *
      * Subprogram for IC206A: literal program name test.
      * Sets a flag and text to confirm it was called.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-FLAG   PIC 9(4).
       01 LS-TEXT   PIC X(10).
       PROCEDURE DIVISION USING LS-FLAG LS-TEXT.
           MOVE 1 TO LS-FLAG.
           MOVE "CALLED    " TO LS-TEXT.
