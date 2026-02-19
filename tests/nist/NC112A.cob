       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC112A.
      *
      * NIST CCVS-style test: EVALUATE statement
      * Tests EVALUATE with multiple WHEN clauses,
      * EVALUATE TRUE, and WHEN OTHER.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CODE     PIC 9(2) VALUE ZEROS.
       01 WS-RESULT   PIC X(10) VALUE SPACES.
       01 WS-FLAG     PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE with multiple WHEN clauses
           MOVE 3 TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-CODE
               WHEN 1
                   MOVE "ONE" TO WS-RESULT
               WHEN 2
                   MOVE "TWO" TO WS-RESULT
               WHEN 3
                   MOVE "THREE" TO WS-RESULT
               WHEN OTHER
                   MOVE "UNKNOWN" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "THREE     "
               DISPLAY "NC112A-TEST-1 PASS"
           ELSE
               DISPLAY "NC112A-TEST-1 FAIL"
               DISPLAY "  Expected THREE, got >" WS-RESULT "<"
           END-IF.
      * Test 2: EVALUATE TRUE
           MOVE 15 TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-CODE < 10
                   MOVE "SMALL" TO WS-RESULT
               WHEN WS-CODE < 20
                   MOVE "MEDIUM" TO WS-RESULT
               WHEN WS-CODE >= 20
                   MOVE "LARGE" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "MEDIUM    "
               DISPLAY "NC112A-TEST-2 PASS"
           ELSE
               DISPLAY "NC112A-TEST-2 FAIL"
               DISPLAY "  Expected MEDIUM, got >" WS-RESULT "<"
           END-IF.
      * Test 3: EVALUATE with WHEN OTHER
           MOVE 99 TO WS-CODE.
           MOVE 0 TO WS-FLAG.
           EVALUATE WS-CODE
               WHEN 1
                   MOVE 1 TO WS-FLAG
               WHEN 2
                   MOVE 2 TO WS-FLAG
               WHEN OTHER
                   MOVE 9 TO WS-FLAG
           END-EVALUATE.
           IF WS-FLAG = 9
               DISPLAY "NC112A-TEST-3 PASS"
           ELSE
               DISPLAY "NC112A-TEST-3 FAIL"
               DISPLAY "  Expected 9, got " WS-FLAG
           END-IF.
           STOP RUN.
