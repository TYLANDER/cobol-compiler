       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC300A.
      *
      * NIST CCVS-style test: Complex EVALUATE with TRUE
      * and range comparisons using relational conditions.
      * Tests EVALUATE TRUE with multiple WHEN conditions
      * that simulate range matching.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SCORE        PIC 9(3) VALUE ZEROS.
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE TRUE - middle range
      *   Score 15 falls in 11-20 range => "MEDIUM"
           MOVE 15 TO WS-SCORE.
           EVALUATE TRUE
               WHEN WS-SCORE >= 1 AND WS-SCORE <= 10
                   MOVE "LOW" TO WS-RESULT
               WHEN WS-SCORE >= 11 AND WS-SCORE <= 20
                   MOVE "MEDIUM" TO WS-RESULT
               WHEN WS-SCORE >= 21 AND WS-SCORE <= 30
                   MOVE "HIGH" TO WS-RESULT
               WHEN OTHER
                   MOVE "OUT-RANGE" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "MEDIUM"
               DISPLAY "NC300A-TEST-1 PASS"
           ELSE
               DISPLAY "NC300A-TEST-1 FAIL"
               DISPLAY "  Expected MEDIUM, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 2: EVALUATE TRUE - boundary value
      *   Score 10 at upper bound of 1-10 => "LOW"
           MOVE 10 TO WS-SCORE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-SCORE >= 1 AND WS-SCORE <= 10
                   MOVE "LOW" TO WS-RESULT
               WHEN WS-SCORE >= 11 AND WS-SCORE <= 20
                   MOVE "MEDIUM" TO WS-RESULT
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "LOW"
               DISPLAY "NC300A-TEST-2 PASS"
           ELSE
               DISPLAY "NC300A-TEST-2 FAIL"
               DISPLAY "  Expected LOW, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 3: EVALUATE TRUE - no range match => OTHER
      *   Score 50 above all ranges => "UNKNOWN"
           MOVE 50 TO WS-SCORE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-SCORE >= 1 AND WS-SCORE <= 10
                   MOVE "LOW" TO WS-RESULT
               WHEN WS-SCORE >= 11 AND WS-SCORE <= 20
                   MOVE "MEDIUM" TO WS-RESULT
               WHEN WS-SCORE >= 21 AND WS-SCORE <= 30
                   MOVE "HIGH" TO WS-RESULT
               WHEN OTHER
                   MOVE "UNKNOWN" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "UNKNOWN"
               DISPLAY "NC300A-TEST-3 PASS"
           ELSE
               DISPLAY "NC300A-TEST-3 FAIL"
               DISPLAY "  Expected UNKNOWN, got >"
                   WS-RESULT "<"
           END-IF.
           STOP RUN.
