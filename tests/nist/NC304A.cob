       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC304A.
      *
      * NIST CCVS-style test: EVALUATE with compound WHEN conditions
      * Tests EVALUATE TRUE with compound boolean expressions
      * using AND and OR in WHEN clauses.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A         PIC 9(2) VALUE ZEROS.
       01 WS-B         PIC 9(2) VALUE ZEROS.
       01 WS-RESULT    PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE TRUE with AND condition
      *   A=5 B=10 => WHEN A > 0 AND B > 5 is true
           MOVE 5 TO WS-A.
           MOVE 10 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-A > 10 AND WS-B > 10
                   MOVE "BOTH-BIG" TO WS-RESULT
               WHEN WS-A > 0 AND WS-B > 5
                   MOVE "MID-RANGE" TO WS-RESULT
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "MID-RANGE"
               DISPLAY "NC304A-TEST-1 PASS"
           ELSE
               DISPLAY "NC304A-TEST-1 FAIL"
               DISPLAY "  Expected MID-RANGE, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 2: EVALUATE TRUE with OR condition
      *   A=0 B=99 => WHEN A = 0 OR B = 99 is true
           MOVE 0 TO WS-A.
           MOVE 99 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-A = 5 AND WS-B = 5
                   MOVE "FIVE-FIVE" TO WS-RESULT
               WHEN WS-A = 0 OR WS-B = 99
                   MOVE "ZERO-OR99" TO WS-RESULT
               WHEN OTHER
                   MOVE "NONE" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "ZERO-OR99"
               DISPLAY "NC304A-TEST-2 PASS"
           ELSE
               DISPLAY "NC304A-TEST-2 FAIL"
               DISPLAY "  Expected ZERO-OR99, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 3: EVALUATE TRUE with compound conditions,
      *   falls to OTHER when none match
      *   A=1 B=1 => neither AND nor OR clauses match
           MOVE 1 TO WS-A.
           MOVE 1 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-A > 50 AND WS-B > 50
                   MOVE "HIGH" TO WS-RESULT
               WHEN WS-A = 0 OR WS-B = 99
                   MOVE "SPECIAL" TO WS-RESULT
               WHEN OTHER
                   MOVE "DEFAULT" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "DEFAULT"
               DISPLAY "NC304A-TEST-3 PASS"
           ELSE
               DISPLAY "NC304A-TEST-3 FAIL"
               DISPLAY "  Expected DEFAULT, got >"
                   WS-RESULT "<"
           END-IF.
           STOP RUN.
