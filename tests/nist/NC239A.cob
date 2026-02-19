       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC239A.
      *
      * NIST CCVS-style test: MOVE CORRESPONDING between
      * group items with matching and non-matching fields.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP-A.
           05 WS-NAME     PIC X(10) VALUE "ALICE".
           05 WS-AGE      PIC 9(3)  VALUE 30.
           05 WS-CITY     PIC X(10) VALUE "BOSTON".
       01 WS-GROUP-B.
           05 WS-NAME     PIC X(10) VALUE SPACES.
           05 WS-AGE      PIC 9(3)  VALUE ZEROS.
           05 WS-DEPT     PIC X(10) VALUE "SALES".
       01 WS-GROUP-C.
           05 WS-NAME     PIC X(5)  VALUE SPACES.
           05 WS-AGE      PIC 9(5)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: MOVE CORRESPONDING between groups with
      *   matching names. NAME and AGE match, should be moved.
           MOVE SPACES TO WS-NAME OF WS-GROUP-B.
           MOVE ZEROS TO WS-AGE OF WS-GROUP-B.
           MOVE "SALES" TO WS-DEPT OF WS-GROUP-B.
           MOVE CORRESPONDING WS-GROUP-A TO WS-GROUP-B.
           IF WS-NAME OF WS-GROUP-B = "ALICE"
               AND WS-AGE OF WS-GROUP-B = 30
               DISPLAY "NC239A-TEST-1 PASS"
           ELSE
               DISPLAY "NC239A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-NAME OF WS-GROUP-B
                   "< AGE=" WS-AGE OF WS-GROUP-B
           END-IF.
      * Test 2: Verify non-matching fields are unchanged
      *   DEPT in GROUP-B has no match in GROUP-A, so stays
           IF WS-DEPT OF WS-GROUP-B = "SALES"
               DISPLAY "NC239A-TEST-2 PASS"
           ELSE
               DISPLAY "NC239A-TEST-2 FAIL"
               DISPLAY "  DEPT should be SALES, got >"
                   WS-DEPT OF WS-GROUP-B "<"
           END-IF.
      * Test 3: MOVE CORRESPONDING with different PIC sizes
      *   GROUP-A NAME is X(10), GROUP-C NAME is X(5)
      *   "ALICE" fits, AGE 30 fits in 9(5)
           MOVE SPACES TO WS-NAME OF WS-GROUP-C.
           MOVE ZEROS TO WS-AGE OF WS-GROUP-C.
           MOVE CORRESPONDING WS-GROUP-A TO WS-GROUP-C.
           IF WS-NAME OF WS-GROUP-C = "ALICE"
               AND WS-AGE OF WS-GROUP-C = 30
               DISPLAY "NC239A-TEST-3 PASS"
           ELSE
               DISPLAY "NC239A-TEST-3 FAIL"
               DISPLAY "  NAME=>" WS-NAME OF WS-GROUP-C
                   "< AGE=" WS-AGE OF WS-GROUP-C
           END-IF.
           STOP RUN.
