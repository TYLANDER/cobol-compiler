       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC218A.
      *
      * NIST CCVS-style test: EVALUATE TRUE with complex conditions
      * Tests EVALUATE TRUE with compound WHEN conditions,
      * multiple WHEN clauses, and nested EVALUATE.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-AGE          PIC 9(3)  VALUE ZEROS.
       01 WS-SCORE        PIC 9(3)  VALUE ZEROS.
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       01 WS-CODE         PIC X(1)  VALUE SPACE.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE TRUE with range conditions
      *   Age=25: should match 18-64 range -> "ADULT"
           MOVE 25 TO WS-AGE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-AGE < 13
                   MOVE "CHILD" TO WS-RESULT
               WHEN WS-AGE < 18
                   MOVE "TEEN" TO WS-RESULT
               WHEN WS-AGE < 65
                   MOVE "ADULT" TO WS-RESULT
               WHEN OTHER
                   MOVE "SENIOR" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT(1:5) = "ADULT"
               DISPLAY "NC218A-TEST-1 PASS"
           ELSE
               DISPLAY "NC218A-TEST-1 FAIL"
               DISPLAY "  RESULT=>" WS-RESULT "<"
           END-IF.
      * Test 2: EVALUATE TRUE with multiple matching tests
      *   Score=85: B range, Age=70: SENIOR
           MOVE 85 TO WS-SCORE.
           MOVE 70 TO WS-AGE.
           MOVE SPACES TO WS-RESULT.
           MOVE SPACE TO WS-CODE.
           EVALUATE TRUE
               WHEN WS-SCORE >= 90
                   MOVE "A" TO WS-CODE
               WHEN WS-SCORE >= 80
                   MOVE "B" TO WS-CODE
               WHEN WS-SCORE >= 70
                   MOVE "C" TO WS-CODE
               WHEN OTHER
                   MOVE "F" TO WS-CODE
           END-EVALUATE.
           EVALUATE TRUE
               WHEN WS-AGE < 18
                   MOVE "YOUNG" TO WS-RESULT
               WHEN WS-AGE >= 65
                   MOVE "SENIOR" TO WS-RESULT
               WHEN OTHER
                   MOVE "MIDDLE" TO WS-RESULT
           END-EVALUATE.
           IF WS-CODE = "B" AND WS-RESULT(1:6) = "SENIOR"
               DISPLAY "NC218A-TEST-2 PASS"
           ELSE
               DISPLAY "NC218A-TEST-2 FAIL"
               DISPLAY "  CODE=" WS-CODE
                   " RESULT=>" WS-RESULT "<"
           END-IF.
      * Test 3: EVALUATE TRUE - boundary test
      *   Score=90 should be A, Score=89 should be B
           MOVE 90 TO WS-SCORE.
           MOVE SPACE TO WS-CODE.
           EVALUATE TRUE
               WHEN WS-SCORE >= 90
                   MOVE "A" TO WS-CODE
               WHEN WS-SCORE >= 80
                   MOVE "B" TO WS-CODE
               WHEN OTHER
                   MOVE "F" TO WS-CODE
           END-EVALUATE.
           IF WS-CODE = "A"
               MOVE 89 TO WS-SCORE
               MOVE SPACE TO WS-CODE
               EVALUATE TRUE
                   WHEN WS-SCORE >= 90
                       MOVE "A" TO WS-CODE
                   WHEN WS-SCORE >= 80
                       MOVE "B" TO WS-CODE
                   WHEN OTHER
                       MOVE "F" TO WS-CODE
               END-EVALUATE
               IF WS-CODE = "B"
                   DISPLAY "NC218A-TEST-3 PASS"
               ELSE
                   DISPLAY "NC218A-TEST-3 FAIL"
                   DISPLAY "  89 gave CODE=" WS-CODE
               END-IF
           ELSE
               DISPLAY "NC218A-TEST-3 FAIL"
               DISPLAY "  90 gave CODE=" WS-CODE
           END-IF.
           STOP RUN.
