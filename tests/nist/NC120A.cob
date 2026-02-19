       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC120A.
      *
      * NIST CCVS-style test: COMPUTE with complex expressions
      * Tests parenthesized expressions, division with rounding,
      * and multi-operator expressions with exponentiation.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A         PIC 9(4) VALUE ZEROS.
       01 WS-B         PIC 9(4) VALUE ZEROS.
       01 WS-C         PIC 9(4) VALUE ZEROS.
       01 WS-D         PIC 9(4) VALUE ZEROS.
       01 WS-RESULT    PIC 9(8) VALUE ZEROS.
       01 WS-DEC-RES   PIC 9(5)V99 VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: COMPUTE X = (A + B) * C
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 3 TO WS-C.
           COMPUTE WS-RESULT = (WS-A + WS-B) * WS-C.
           IF WS-RESULT = 90
               DISPLAY "NC120A-TEST-1 PASS"
           ELSE
               DISPLAY "NC120A-TEST-1 FAIL"
               DISPLAY "  Expected 90, got " WS-RESULT
           END-IF.
      * Test 2: COMPUTE with division (integer result)
      *   100 / 4 = 25
           MOVE 100 TO WS-A.
           MOVE 4 TO WS-B.
           COMPUTE WS-RESULT = WS-A / WS-B.
           IF WS-RESULT = 25
               DISPLAY "NC120A-TEST-2 PASS"
           ELSE
               DISPLAY "NC120A-TEST-2 FAIL"
               DISPLAY "  Expected 25, got " WS-RESULT
           END-IF.
      * Test 3: COMPUTE with multiple operators
      *   X = A ** 2 + B * C - D
      *   A=3, B=4, C=5, D=2  =>  9 + 20 - 2 = 27
           MOVE 3 TO WS-A.
           MOVE 4 TO WS-B.
           MOVE 5 TO WS-C.
           MOVE 2 TO WS-D.
           COMPUTE WS-RESULT = WS-A ** 2 + WS-B * WS-C - WS-D.
           IF WS-RESULT = 27
               DISPLAY "NC120A-TEST-3 PASS"
           ELSE
               DISPLAY "NC120A-TEST-3 FAIL"
               DISPLAY "  Expected 27, got " WS-RESULT
           END-IF.
           STOP RUN.
