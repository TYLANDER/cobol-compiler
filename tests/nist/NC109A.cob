       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC109A.
      *
      * NIST CCVS-style test: IF statement variations
      * Tests NOT, AND, OR, nested IF, and comparisons
      * across different-sized fields.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC 9(4) VALUE ZEROS.
       01 WS-B        PIC 9(4) VALUE ZEROS.
       01 WS-SMALL    PIC 9(2) VALUE ZEROS.
       01 WS-BIG      PIC 9(8) VALUE ZEROS.
       01 WS-RESULT   PIC X(4) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: IF with NOT
           MOVE 10 TO WS-A.
           IF NOT WS-A = 5
               DISPLAY "NC109A-TEST-1 PASS"
           ELSE
               DISPLAY "NC109A-TEST-1 FAIL"
               DISPLAY "  WS-A is " WS-A " expected NOT = 5"
           END-IF.
      * Test 2: IF with AND
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           IF WS-A > 0 AND WS-B > 0
               DISPLAY "NC109A-TEST-2 PASS"
           ELSE
               DISPLAY "NC109A-TEST-2 FAIL"
               DISPLAY "  WS-A=" WS-A " WS-B=" WS-B
           END-IF.
      * Test 3: IF with OR
           MOVE 2 TO WS-A.
           IF WS-A = 1 OR WS-A = 2
               DISPLAY "NC109A-TEST-3 PASS"
           ELSE
               DISPLAY "NC109A-TEST-3 FAIL"
               DISPLAY "  WS-A=" WS-A " expected 1 or 2"
           END-IF.
      * Test 4: Nested IF
           MOVE 5 TO WS-A.
           MOVE 10 TO WS-B.
           MOVE "FAIL" TO WS-RESULT.
           IF WS-A > 0
               IF WS-B > WS-A
                   MOVE "PASS" TO WS-RESULT
               END-IF
           END-IF.
           IF WS-RESULT = "PASS"
               DISPLAY "NC109A-TEST-4 PASS"
           ELSE
               DISPLAY "NC109A-TEST-4 FAIL"
               DISPLAY "  Nested IF did not execute correctly"
           END-IF.
      * Test 5: Numeric comparison across different-sized fields
           MOVE 50 TO WS-SMALL.
           MOVE 50 TO WS-BIG.
           IF WS-SMALL = WS-BIG
               DISPLAY "NC109A-TEST-5 PASS"
           ELSE
               DISPLAY "NC109A-TEST-5 FAIL"
               DISPLAY "  WS-SMALL=" WS-SMALL
               DISPLAY "  WS-BIG=" WS-BIG
           END-IF.
           STOP RUN.
