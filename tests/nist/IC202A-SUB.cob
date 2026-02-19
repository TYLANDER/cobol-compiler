       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC202A-SUB.
      *
      * Subprogram for IC202A: BY CONTENT parameter test.
      * Reads content parameters and sets an output flag to
      * confirm the values were received correctly.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-ALPHA  PIC X(10).
       01 LS-NUM    PIC 9(4).
       01 LS-FLAG   PIC 9(4).
       PROCEDURE DIVISION USING LS-ALPHA LS-NUM LS-FLAG.
           IF LS-ALPHA = "ORIGINAL  " AND LS-NUM = 5555
               MOVE 1 TO LS-FLAG
           ELSE
               MOVE 0 TO LS-FLAG
           END-IF.
