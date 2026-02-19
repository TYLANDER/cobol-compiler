       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC199A.
      *
      * NIST CCVS-style test: COMPUTE with exponentiation
      * Tests the ** operator with various bases and exponents.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A          PIC 9(4) VALUE ZEROS.
       01 WS-B          PIC 9(4) VALUE ZEROS.
       01 WS-RESULT     PIC 9(8) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Simple exponentiation - 5 ** 3 = 125
           MOVE 5 TO WS-A.
           COMPUTE WS-RESULT = WS-A ** 3.
           IF WS-RESULT = 125
               DISPLAY "NC199A-TEST-1 PASS"
           ELSE
               DISPLAY "NC199A-TEST-1 FAIL"
               DISPLAY "  Expected 125, got " WS-RESULT
           END-IF.
      * Test 2: Exponentiation with 2 ** 10 = 1024
           COMPUTE WS-RESULT = 2 ** 10.
           IF WS-RESULT = 1024
               DISPLAY "NC199A-TEST-2 PASS"
           ELSE
               DISPLAY "NC199A-TEST-2 FAIL"
               DISPLAY "  Expected 1024, got " WS-RESULT
           END-IF.
      * Test 3: Exponentiation with variable exponent
      *   3 ** 4 = 81
           MOVE 3 TO WS-A.
           MOVE 4 TO WS-B.
           COMPUTE WS-RESULT = WS-A ** WS-B.
           IF WS-RESULT = 81
               DISPLAY "NC199A-TEST-3 PASS"
           ELSE
               DISPLAY "NC199A-TEST-3 FAIL"
               DISPLAY "  Expected 81, got " WS-RESULT
           END-IF.
           STOP RUN.
