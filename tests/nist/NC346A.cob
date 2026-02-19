       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC346A.
      *
      * NIST CCVS-style test: Nested IF with ELSE matching
      * Tests correct association of ELSE with the nearest IF
      * (the dangling else problem) and deep nesting.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(2) VALUE 0.
       01 WS-B            PIC 9(2) VALUE 0.
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Nested IF - inner ELSE matches inner IF
      *   A=5, B=3: outer IF true, inner IF true => "INNER-T"
           MOVE 5 TO WS-A.
           MOVE 3 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           IF WS-A > 0
               IF WS-B > 0
                   MOVE "INNER-T" TO WS-RESULT
               ELSE
                   MOVE "INNER-F" TO WS-RESULT
               END-IF
           ELSE
               MOVE "OUTER-F" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "INNER-T   "
               DISPLAY "NC346A-TEST-1 PASS"
           ELSE
               DISPLAY "NC346A-TEST-1 FAIL"
               DISPLAY "  Expected INNER-T, got >" WS-RESULT "<"
           END-IF.
      * Test 2: Nested IF - outer true, inner false
      *   A=5, B=0: outer IF true, inner IF false => "INNER-F"
           MOVE 5 TO WS-A.
           MOVE 0 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           IF WS-A > 0
               IF WS-B > 0
                   MOVE "INNER-T" TO WS-RESULT
               ELSE
                   MOVE "INNER-F" TO WS-RESULT
               END-IF
           ELSE
               MOVE "OUTER-F" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "INNER-F   "
               DISPLAY "NC346A-TEST-2 PASS"
           ELSE
               DISPLAY "NC346A-TEST-2 FAIL"
               DISPLAY "  Expected INNER-F, got >" WS-RESULT "<"
           END-IF.
      * Test 3: Nested IF - outer false, skips inner entirely
      *   A=0: outer IF false => "OUTER-F"
           MOVE 0 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           IF WS-A > 0
               IF WS-B > 0
                   MOVE "INNER-T" TO WS-RESULT
               ELSE
                   MOVE "INNER-F" TO WS-RESULT
               END-IF
           ELSE
               MOVE "OUTER-F" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "OUTER-F   "
               DISPLAY "NC346A-TEST-3 PASS"
           ELSE
               DISPLAY "NC346A-TEST-3 FAIL"
               DISPLAY "  Expected OUTER-F, got >" WS-RESULT "<"
           END-IF.
           STOP RUN.
