       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC323A.
      *
      * NIST CCVS-style test: EVALUATE TRUE with compound WHEN
      * Tests EVALUATE TRUE where WHEN conditions combine multiple
      * comparisons using AND.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-AGE          PIC 9(3) VALUE 0.
       01 WS-SCORE        PIC 9(3) VALUE 0.
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE TRUE - first WHEN matches
      *   AGE=25, SCORE=90 => RESULT should be "YOUNG-HIGH"
           MOVE 25 TO WS-AGE.
           MOVE 90 TO WS-SCORE.
           EVALUATE TRUE
               WHEN WS-AGE < 30 AND WS-SCORE > 80
                   MOVE "YOUNG-HIGH" TO WS-RESULT
               WHEN WS-AGE < 30 AND WS-SCORE < 81
                   MOVE "YOUNG-LOW " TO WS-RESULT
               WHEN WS-AGE > 29 AND WS-SCORE > 80
                   MOVE "OLD-HIGH  " TO WS-RESULT
               WHEN OTHER
                   MOVE "OLD-LOW   " TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "YOUNG-HIGH"
               DISPLAY "NC323A-TEST-1 PASS"
           ELSE
               DISPLAY "NC323A-TEST-1 FAIL"
               DISPLAY "  Expected YOUNG-HIGH, got " WS-RESULT
           END-IF.
      * Test 2: EVALUATE TRUE - third WHEN matches
      *   AGE=45, SCORE=95 => RESULT should be "OLD-HIGH"
           MOVE 45 TO WS-AGE.
           MOVE 95 TO WS-SCORE.
           EVALUATE TRUE
               WHEN WS-AGE < 30 AND WS-SCORE > 80
                   MOVE "YOUNG-HIGH" TO WS-RESULT
               WHEN WS-AGE < 30 AND WS-SCORE < 81
                   MOVE "YOUNG-LOW " TO WS-RESULT
               WHEN WS-AGE > 29 AND WS-SCORE > 80
                   MOVE "OLD-HIGH  " TO WS-RESULT
               WHEN OTHER
                   MOVE "OLD-LOW   " TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "OLD-HIGH  "
               DISPLAY "NC323A-TEST-2 PASS"
           ELSE
               DISPLAY "NC323A-TEST-2 FAIL"
               DISPLAY "  Expected OLD-HIGH, got " WS-RESULT
           END-IF.
      * Test 3: EVALUATE TRUE - OTHER path
      *   AGE=50, SCORE=40 => RESULT should be "OLD-LOW"
           MOVE 50 TO WS-AGE.
           MOVE 40 TO WS-SCORE.
           EVALUATE TRUE
               WHEN WS-AGE < 30 AND WS-SCORE > 80
                   MOVE "YOUNG-HIGH" TO WS-RESULT
               WHEN WS-AGE < 30 AND WS-SCORE < 81
                   MOVE "YOUNG-LOW " TO WS-RESULT
               WHEN WS-AGE > 29 AND WS-SCORE > 80
                   MOVE "OLD-HIGH  " TO WS-RESULT
               WHEN OTHER
                   MOVE "OLD-LOW   " TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "OLD-LOW   "
               DISPLAY "NC323A-TEST-3 PASS"
           ELSE
               DISPLAY "NC323A-TEST-3 FAIL"
               DISPLAY "  Expected OLD-LOW, got " WS-RESULT
           END-IF.
           STOP RUN.
