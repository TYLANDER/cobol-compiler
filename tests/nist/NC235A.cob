       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC235A.
      *
      * NIST CCVS-style test: EVALUATE TRUE with multiple
      * WHEN clauses and WHEN OTHER default branch.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SCORE    PIC 9(3) VALUE ZEROS.
       01 WS-GRADE    PIC X(4) VALUE SPACES.
       01 WS-CODE     PIC 9    VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE TRUE with WHEN condition
      *   Score = 85, should match >= 80 branch
           MOVE 85 TO WS-SCORE.
           MOVE SPACES TO WS-GRADE.
           EVALUATE TRUE
               WHEN WS-SCORE >= 90
                   MOVE "A" TO WS-GRADE
               WHEN WS-SCORE >= 80
                   MOVE "B" TO WS-GRADE
               WHEN WS-SCORE >= 70
                   MOVE "C" TO WS-GRADE
               WHEN OTHER
                   MOVE "F" TO WS-GRADE
           END-EVALUATE.
           IF WS-GRADE = "B"
               DISPLAY "NC235A-TEST-1 PASS"
           ELSE
               DISPLAY "NC235A-TEST-1 FAIL"
               DISPLAY "  Expected B, got " WS-GRADE
           END-IF.
      * Test 2: EVALUATE with WHEN OTHER default
      *   Score = 50, should fall through to WHEN OTHER
           MOVE 50 TO WS-SCORE.
           MOVE SPACES TO WS-GRADE.
           EVALUATE TRUE
               WHEN WS-SCORE >= 90
                   MOVE "A" TO WS-GRADE
               WHEN WS-SCORE >= 80
                   MOVE "B" TO WS-GRADE
               WHEN WS-SCORE >= 70
                   MOVE "C" TO WS-GRADE
               WHEN OTHER
                   MOVE "F" TO WS-GRADE
           END-EVALUATE.
           IF WS-GRADE = "F"
               DISPLAY "NC235A-TEST-2 PASS"
           ELSE
               DISPLAY "NC235A-TEST-2 FAIL"
               DISPLAY "  Expected F, got " WS-GRADE
           END-IF.
      * Test 3: EVALUATE with multiple WHEN matching first
      *   Code = 1, first WHEN matches, second should not
           MOVE 1 TO WS-CODE.
           MOVE SPACES TO WS-GRADE.
           EVALUATE TRUE
               WHEN WS-CODE = 1
                   MOVE "ONE" TO WS-GRADE
               WHEN WS-CODE = 1
                   MOVE "DUP" TO WS-GRADE
               WHEN WS-CODE = 2
                   MOVE "TWO" TO WS-GRADE
               WHEN OTHER
                   MOVE "OTH" TO WS-GRADE
           END-EVALUATE.
           IF WS-GRADE = "ONE"
               DISPLAY "NC235A-TEST-3 PASS"
           ELSE
               DISPLAY "NC235A-TEST-3 FAIL"
               DISPLAY "  Expected ONE, got " WS-GRADE
           END-IF.
           STOP RUN.
