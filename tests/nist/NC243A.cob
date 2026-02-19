       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC243A.
      *
      * NIST CCVS-style test: Nested PERFORM UNTIL loops
      * Tests nested inline PERFORM UNTIL with counters.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I        PIC 9(4) VALUE ZEROS.
       01 WS-J        PIC 9(4) VALUE ZEROS.
       01 WS-TOTAL    PIC 9(4) VALUE ZEROS.
       01 WS-OUTER    PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Simple nested PERFORM UNTIL
      *   Outer loops 3 times, inner loops 4 times each
      *   Total iterations = 3 * 4 = 12
           MOVE 0 TO WS-TOTAL.
           MOVE 1 TO WS-I.
           PERFORM UNTIL WS-I > 3
               MOVE 1 TO WS-J
               PERFORM UNTIL WS-J > 4
                   ADD 1 TO WS-TOTAL
                   ADD 1 TO WS-J
               END-PERFORM
               ADD 1 TO WS-I
           END-PERFORM.
           IF WS-TOTAL = 12
               DISPLAY "NC243A-TEST-1 PASS"
           ELSE
               DISPLAY "NC243A-TEST-1 FAIL"
               DISPLAY "  Expected 12, got " WS-TOTAL
           END-IF.
      * Test 2: Nested loops accumulating sum of products
      *   Sum of (i * j) for i=1..2, j=1..3
      *   = (1*1)+(1*2)+(1*3)+(2*1)+(2*2)+(2*3)
      *   = 1+2+3+2+4+6 = 18
           MOVE 0 TO WS-TOTAL.
           MOVE 1 TO WS-I.
           PERFORM UNTIL WS-I > 2
               MOVE 1 TO WS-J
               PERFORM UNTIL WS-J > 3
                   COMPUTE WS-OUTER = WS-I * WS-J
                   ADD WS-OUTER TO WS-TOTAL
                   ADD 1 TO WS-J
               END-PERFORM
               ADD 1 TO WS-I
           END-PERFORM.
           IF WS-TOTAL = 18
               DISPLAY "NC243A-TEST-2 PASS"
           ELSE
               DISPLAY "NC243A-TEST-2 FAIL"
               DISPLAY "  Expected 18, got " WS-TOTAL
           END-IF.
      * Test 3: Three-level nesting
      *   Each level loops 2 times => 2 * 2 * 2 = 8
           MOVE 0 TO WS-TOTAL.
           MOVE 0 TO WS-I.
           PERFORM UNTIL WS-I = 2
               MOVE 0 TO WS-J
               PERFORM UNTIL WS-J = 2
                   MOVE 0 TO WS-OUTER
                   PERFORM UNTIL WS-OUTER = 2
                       ADD 1 TO WS-TOTAL
                       ADD 1 TO WS-OUTER
                   END-PERFORM
                   ADD 1 TO WS-J
               END-PERFORM
               ADD 1 TO WS-I
           END-PERFORM.
           IF WS-TOTAL = 8
               DISPLAY "NC243A-TEST-3 PASS"
           ELSE
               DISPLAY "NC243A-TEST-3 FAIL"
               DISPLAY "  Expected 8, got " WS-TOTAL
           END-IF.
           STOP RUN.
