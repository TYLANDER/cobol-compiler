       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC298A.
      *
      * NIST CCVS-style test: COMPUTE with exponentiation
      * Tests the ** (power) operator in COMPUTE statements.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-BASE         PIC 9(4) VALUE ZEROS.
       01 WS-RESULT       PIC 9(8) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Square a number (A ** 2)
      *   5 ** 2 = 25
           MOVE 5 TO WS-BASE.
           COMPUTE WS-RESULT = WS-BASE ** 2.
           IF WS-RESULT = 25
               DISPLAY "NC298A-TEST-1 PASS"
           ELSE
               DISPLAY "NC298A-TEST-1 FAIL"
               DISPLAY "  Expected 25, got " WS-RESULT
           END-IF.
      * Test 2: Cube a number (A ** 3)
      *   4 ** 3 = 64
           MOVE 4 TO WS-BASE.
           COMPUTE WS-RESULT = WS-BASE ** 3.
           IF WS-RESULT = 64
               DISPLAY "NC298A-TEST-2 PASS"
           ELSE
               DISPLAY "NC298A-TEST-2 FAIL"
               DISPLAY "  Expected 64, got " WS-RESULT
           END-IF.
      * Test 3: Exponentiation in expression
      *   3 ** 2 + 4 ** 2 = 9 + 16 = 25
           COMPUTE WS-RESULT = 3 ** 2 + 4 ** 2.
           IF WS-RESULT = 25
               DISPLAY "NC298A-TEST-3 PASS"
           ELSE
               DISPLAY "NC298A-TEST-3 FAIL"
               DISPLAY "  Expected 25, got " WS-RESULT
           END-IF.
           STOP RUN.
