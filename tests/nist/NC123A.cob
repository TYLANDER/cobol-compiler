       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC123A.
      *
      * NIST CCVS-style test: EVALUATE statement edge cases
      * Tests EVALUATE TRUE with multiple WHEN clauses,
      * EVALUATE var WHEN OTHER, and compound conditions.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR       PIC 9(4) VALUE ZEROS.
       01 WS-RESULT    PIC X(10) VALUE SPACES.
       01 WS-A         PIC 9(4) VALUE ZEROS.
       01 WS-B         PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE TRUE with multiple WHEN clauses
      *   Set WS-VAR to 2 so second WHEN matches (first match wins)
           MOVE 2 TO WS-VAR.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-VAR = 1
                   MOVE "FIRST" TO WS-RESULT
               WHEN WS-VAR = 2
                   MOVE "SECOND" TO WS-RESULT
               WHEN WS-VAR = 3
                   MOVE "THIRD" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "SECOND    "
               DISPLAY "NC123A-TEST-1 PASS"
           ELSE
               DISPLAY "NC123A-TEST-1 FAIL"
               DISPLAY "  Expected SECOND, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 2: EVALUATE var WHEN OTHER (default case)
      *   Set WS-VAR to 99 which matches no explicit WHEN
           MOVE 99 TO WS-VAR.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-VAR
               WHEN 1
                   MOVE "ONE" TO WS-RESULT
               WHEN 2
                   MOVE "TWO" TO WS-RESULT
               WHEN OTHER
                   MOVE "DEFAULT" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "DEFAULT   "
               DISPLAY "NC123A-TEST-2 PASS"
           ELSE
               DISPLAY "NC123A-TEST-2 FAIL"
               DISPLAY "  Expected DEFAULT, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 3: EVALUATE TRUE with compound conditions (AND/OR)
      *   WS-A=5 and WS-B=10: first WHEN needs both > 0 AND < 20
           MOVE 5 TO WS-A.
           MOVE 10 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-A > 0 AND WS-A < 20
                   IF WS-B > 0 AND WS-B < 20
                       MOVE "BOTH" TO WS-RESULT
                   ELSE
                       MOVE "AONLY" TO WS-RESULT
                   END-IF
               WHEN OTHER
                   MOVE "NONE" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "BOTH      "
               DISPLAY "NC123A-TEST-3 PASS"
           ELSE
               DISPLAY "NC123A-TEST-3 FAIL"
               DISPLAY "  Expected BOTH, got >"
                   WS-RESULT "<"
           END-IF.
           STOP RUN.
