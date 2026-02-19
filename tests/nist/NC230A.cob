       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC230A.
      *
      * NIST CCVS-style test: COMPUTE with subtraction and
      * multiplication. Avoids division and exponentiation of
      * negative numbers. Tests operator precedence and
      * parenthesized expressions.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A             PIC 9(4) VALUE ZEROS.
       01 WS-B             PIC 9(4) VALUE ZEROS.
       01 WS-C             PIC 9(4) VALUE ZEROS.
       01 WS-RESULT        PIC 9(8) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: COMPUTE with multiplication and subtraction
      *   COMPUTE R = 10 * 5 - 3
      *   10 * 5 = 50, 50 - 3 = 47
           COMPUTE WS-RESULT = 10 * 5 - 3.
           IF WS-RESULT = 47
               DISPLAY "NC230A-TEST-1 PASS"
           ELSE
               DISPLAY "NC230A-TEST-1 FAIL"
               DISPLAY "  Expected 47, got " WS-RESULT
           END-IF.
      * Test 2: COMPUTE with parenthesized subtraction
      *   COMPUTE R = 10 * (5 - 3)
      *   (5 - 3) = 2, 10 * 2 = 20
           COMPUTE WS-RESULT = 10 * (5 - 3).
           IF WS-RESULT = 20
               DISPLAY "NC230A-TEST-2 PASS"
           ELSE
               DISPLAY "NC230A-TEST-2 FAIL"
               DISPLAY "  Expected 20, got " WS-RESULT
           END-IF.
      * Test 3: COMPUTE with variables, multiply and subtract
      *   A=8, B=6, C=3
      *   COMPUTE R = A * B - C * 2
      *   8*6 = 48, 3*2 = 6, 48 - 6 = 42
           MOVE 8 TO WS-A.
           MOVE 6 TO WS-B.
           MOVE 3 TO WS-C.
           COMPUTE WS-RESULT = WS-A * WS-B - WS-C * 2.
           IF WS-RESULT = 42
               DISPLAY "NC230A-TEST-3 PASS"
           ELSE
               DISPLAY "NC230A-TEST-3 FAIL"
               DISPLAY "  Expected 42, got " WS-RESULT
           END-IF.
           STOP RUN.
