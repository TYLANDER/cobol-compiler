       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC381A.
      *
      * NIST CCVS-style test: Nested IF with multiple ELSE branches
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A               PIC 9     VALUE 0.
       01 WS-B               PIC 9     VALUE 0.
       01 WS-RESULT          PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Outer IF true, inner IF true
           MOVE 5 TO WS-A.
           MOVE 3 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           IF WS-A > 3
               IF WS-B > 2
                   MOVE "BOTH-TRUE" TO WS-RESULT
               ELSE
                   MOVE "INNER-NO" TO WS-RESULT
               END-IF
           ELSE
               MOVE "OUTER-NO" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "BOTH-TRUE "
               DISPLAY "NC381A-TEST-1 PASS"
           ELSE
               DISPLAY "NC381A-TEST-1 FAIL"
               DISPLAY "  Expected BOTH-TRUE, got " WS-RESULT
           END-IF.
      * Test 2: Outer IF true, inner IF false
           MOVE 5 TO WS-A.
           MOVE 1 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           IF WS-A > 3
               IF WS-B > 2
                   MOVE "BOTH-TRUE" TO WS-RESULT
               ELSE
                   MOVE "INNER-NO" TO WS-RESULT
               END-IF
           ELSE
               MOVE "OUTER-NO" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "INNER-NO  "
               DISPLAY "NC381A-TEST-2 PASS"
           ELSE
               DISPLAY "NC381A-TEST-2 FAIL"
               DISPLAY "  Expected INNER-NO, got " WS-RESULT
           END-IF.
      * Test 3: Outer IF false (skips inner entirely)
           MOVE 1 TO WS-A.
           MOVE 9 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           IF WS-A > 3
               IF WS-B > 2
                   MOVE "BOTH-TRUE" TO WS-RESULT
               ELSE
                   MOVE "INNER-NO" TO WS-RESULT
               END-IF
           ELSE
               MOVE "OUTER-NO" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "OUTER-NO  "
               DISPLAY "NC381A-TEST-3 PASS"
           ELSE
               DISPLAY "NC381A-TEST-3 FAIL"
               DISPLAY "  Expected OUTER-NO, got " WS-RESULT
           END-IF.
           STOP RUN.
