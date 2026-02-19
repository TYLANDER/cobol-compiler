       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC155A.
      *
      * NIST CCVS-style test: EVALUATE TRUE/FALSE with WHEN OTHER
      * Tests EVALUATE TRUE and EVALUATE FALSE with multiple WHEN
      * clauses including compound conditions and WHEN OTHER.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-X             PIC 9(4) VALUE ZEROS.
       01 WS-Y             PIC 9(4) VALUE ZEROS.
       01 WS-RESULT        PIC X(10) VALUE SPACES.
       01 WS-FLAG          PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE TRUE with multiple WHEN — first match wins
      *   X=5 matches both "< 10" and "< 20" but first WHEN wins
           MOVE 5 TO WS-X.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-X < 10
                   MOVE "SMALL" TO WS-RESULT
               WHEN WS-X < 20
                   MOVE "MEDIUM" TO WS-RESULT
               WHEN WS-X < 100
                   MOVE "LARGE" TO WS-RESULT
               WHEN OTHER
                   MOVE "HUGE" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT(1:5) = "SMALL"
               DISPLAY "NC155A-TEST-1 PASS"
           ELSE
               DISPLAY "NC155A-TEST-1 FAIL"
               DISPLAY "  Expected SMALL, got >" WS-RESULT "<"
           END-IF.
      * Test 2: EVALUATE FALSE — selects clause where condition
      *   is false
      *   X=15 => "X < 10" is FALSE => matches first WHEN
           MOVE 15 TO WS-X.
           MOVE 0 TO WS-FLAG.
           EVALUATE FALSE
               WHEN WS-X < 10
                   MOVE 1 TO WS-FLAG
               WHEN WS-X > 100
                   MOVE 2 TO WS-FLAG
               WHEN OTHER
                   MOVE 9 TO WS-FLAG
           END-EVALUATE.
           IF WS-FLAG = 1
               DISPLAY "NC155A-TEST-2 PASS"
           ELSE
               DISPLAY "NC155A-TEST-2 FAIL"
               DISPLAY "  Expected FLAG=1, got " WS-FLAG
           END-IF.
      * Test 3: EVALUATE TRUE — all conditions false, WHEN OTHER
      *   X=999 does not match any WHEN condition
           MOVE 999 TO WS-X.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-X = 1
                   MOVE "ONE" TO WS-RESULT
               WHEN WS-X = 2
                   MOVE "TWO" TO WS-RESULT
               WHEN WS-X = 3
                   MOVE "THREE" TO WS-RESULT
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT(1:5) = "OTHER"
               DISPLAY "NC155A-TEST-3 PASS"
           ELSE
               DISPLAY "NC155A-TEST-3 FAIL"
               DISPLAY "  Expected OTHER, got >" WS-RESULT "<"
           END-IF.
           STOP RUN.
