       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC160A.
      *
      * NIST CCVS-style test: COMPUTE with complex expressions
      * Tests COMPUTE with multiple operators, parentheses,
      * division, and exponentiation to verify order of operations.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A             PIC 9(4) VALUE ZEROS.
       01 WS-B             PIC 9(4) VALUE ZEROS.
       01 WS-C             PIC 9(4) VALUE ZEROS.
       01 WS-RESULT        PIC 9(6) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: COMPUTE with all four basic operators
      *   RESULT = A + B * C - A / A
      *   A=10, B=3, C=4
      *   = 10 + 3*4 - 10/10 = 10 + 12 - 1 = 21
           MOVE 10 TO WS-A.
           MOVE 3 TO WS-B.
           MOVE 4 TO WS-C.
           COMPUTE WS-RESULT = WS-A + WS-B * WS-C
               - WS-A / WS-A.
           IF WS-RESULT = 21
               DISPLAY "NC160A-TEST-1 PASS"
           ELSE
               DISPLAY "NC160A-TEST-1 FAIL"
               DISPLAY "  Expected 21, got " WS-RESULT
           END-IF.
      * Test 2: COMPUTE with nested parentheses
      *   RESULT = ((A + B) * (C - A)) * 2
      *   A=2, B=3, C=7
      *   = ((2+3) * (7-2)) * 2 = (5 * 5) * 2 = 25 * 2 = 50
           MOVE 2 TO WS-A.
           MOVE 3 TO WS-B.
           MOVE 7 TO WS-C.
           COMPUTE WS-RESULT =
               ((WS-A + WS-B) * (WS-C - WS-A)) * 2.
           IF WS-RESULT = 50
               DISPLAY "NC160A-TEST-2 PASS"
           ELSE
               DISPLAY "NC160A-TEST-2 FAIL"
               DISPLAY "  Expected 50, got " WS-RESULT
           END-IF.
      * Test 3: COMPUTE with exponentiation
      *   RESULT = A ** 3 + B ** 2
      *   A=3, B=4 => 27 + 16 = 43
           MOVE 3 TO WS-A.
           MOVE 4 TO WS-B.
           COMPUTE WS-RESULT = WS-A ** 3 + WS-B ** 2.
           IF WS-RESULT = 43
               DISPLAY "NC160A-TEST-3 PASS"
           ELSE
               DISPLAY "NC160A-TEST-3 FAIL"
               DISPLAY "  Expected 43, got " WS-RESULT
           END-IF.
           STOP RUN.
