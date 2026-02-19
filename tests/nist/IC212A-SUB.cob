       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC212A-SUB.
      *
      * Subprogram for IC212A: BY CONTENT parameter test.
      * Reads the BY CONTENT parameters to verify values were
      * received correctly. Sets a flag via BY REFERENCE output.
      * NOTE: Does not attempt to modify the BY CONTENT params
      * to avoid issues if BY CONTENT is treated as BY REFERENCE.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-ALPHA  PIC X(10).
       01 LS-NUM    PIC 9(4).
       01 LS-FLAG   PIC 9(4).
       PROCEDURE DIVISION USING LS-ALPHA LS-NUM LS-FLAG.
           IF LS-ALPHA = "TESTVALUE " AND LS-NUM = 1234
               MOVE 1 TO LS-FLAG
           ELSE
               MOVE 0 TO LS-FLAG
           END-IF.
           GOBACK.
