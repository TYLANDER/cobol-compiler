       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC343A.
      *
      * NIST CCVS-style test: COMPUTE with complex arithmetic
      * Tests parenthesized sub-expressions and multiple operators
      * in a single COMPUTE statement.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(4) VALUE 0.
       01 WS-B            PIC 9(4) VALUE 0.
       01 WS-C            PIC 9(4) VALUE 0.
       01 WS-RESULT       PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: COMPUTE with parenthesized addition
      *   RESULT = (10 + 20) * 3 = 90
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 3 TO WS-C.
           COMPUTE WS-RESULT = (WS-A + WS-B) * WS-C.
           IF WS-RESULT = 90
               DISPLAY "NC343A-TEST-1 PASS"
           ELSE
               DISPLAY "NC343A-TEST-1 FAIL"
               DISPLAY "  Expected 90, got " WS-RESULT
           END-IF.
      * Test 2: COMPUTE with nested parentheses
      *   RESULT = (5 + 3) * (12 - 4) = 8 * 8 = 64
           COMPUTE WS-RESULT = (5 + 3) * (12 - 4).
           IF WS-RESULT = 64
               DISPLAY "NC343A-TEST-2 PASS"
           ELSE
               DISPLAY "NC343A-TEST-2 FAIL"
               DISPLAY "  Expected 64, got " WS-RESULT
           END-IF.
      * Test 3: COMPUTE with multiple operators and precedence
      *   RESULT = 100 - 4 * 5 + 10 = 100 - 20 + 10 = 90
           COMPUTE WS-RESULT = 100 - 4 * 5 + 10.
           IF WS-RESULT = 90
               DISPLAY "NC343A-TEST-3 PASS"
           ELSE
               DISPLAY "NC343A-TEST-3 FAIL"
               DISPLAY "  Expected 90, got " WS-RESULT
           END-IF.
           STOP RUN.
