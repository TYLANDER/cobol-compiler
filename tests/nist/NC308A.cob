       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC308A.
      *
      * NIST CCVS-style test: IF with NOT conditions
      * Tests NOT EQUAL, NOT GREATER, and NOT LESS conditions
      * for correct boolean evaluation.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A         PIC 9(4) VALUE ZEROS.
       01 WS-B         PIC 9(4) VALUE ZEROS.
       01 WS-TEXT       PIC X(5) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: IF NOT EQUAL
      *   A=10 B=20 => A is NOT EQUAL TO B => true
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           IF WS-A NOT EQUAL WS-B
               DISPLAY "NC308A-TEST-1 PASS"
           ELSE
               DISPLAY "NC308A-TEST-1 FAIL"
               DISPLAY "  10 NOT EQUAL 20 should be true"
           END-IF.
      * Test 2: IF NOT GREATER
      *   A=5 B=10 => A is NOT GREATER THAN B => true
      *   Also test NOT GREATER when equal
           MOVE 5 TO WS-A.
           MOVE 10 TO WS-B.
           IF WS-A NOT GREATER THAN WS-B
               MOVE 10 TO WS-A
               IF WS-A NOT GREATER THAN WS-B
                   DISPLAY "NC308A-TEST-2 PASS"
               ELSE
                   DISPLAY "NC308A-TEST-2 FAIL"
                   DISPLAY "  10 NOT GREATER 10 should"
                       " be true"
               END-IF
           ELSE
               DISPLAY "NC308A-TEST-2 FAIL"
               DISPLAY "  5 NOT GREATER 10 should be true"
           END-IF.
      * Test 3: IF NOT LESS
      *   A=20 B=10 => A NOT LESS THAN B => true
      *   Then A=10 B=10 => A NOT LESS THAN B => true (equal)
           MOVE 20 TO WS-A.
           MOVE 10 TO WS-B.
           IF WS-A NOT LESS THAN WS-B
               MOVE 10 TO WS-A
               IF WS-A NOT LESS THAN WS-B
                   DISPLAY "NC308A-TEST-3 PASS"
               ELSE
                   DISPLAY "NC308A-TEST-3 FAIL"
                   DISPLAY "  10 NOT LESS 10 should be true"
               END-IF
           ELSE
               DISPLAY "NC308A-TEST-3 FAIL"
               DISPLAY "  20 NOT LESS 10 should be true"
           END-IF.
           STOP RUN.
