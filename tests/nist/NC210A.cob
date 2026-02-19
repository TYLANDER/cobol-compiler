       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC210A.
      *
      * NIST CCVS-style test: COMPUTE with complex precedence
      * Tests operator precedence in COMPUTE: exponentiation binds
      * tighter than multiplication/division, which bind tighter
      * than addition/subtraction. Also tests deeply nested
      * parentheses overriding default precedence.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(4)  VALUE ZEROS.
       01 WS-B            PIC 9(4)  VALUE ZEROS.
       01 WS-C            PIC 9(4)  VALUE ZEROS.
       01 WS-RESULT       PIC 9(8)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Precedence: ** before * before +
      *   COMPUTE R = 2 + 3 * 4 ** 2
      *   4 ** 2 = 16, 3 * 16 = 48, 2 + 48 = 50
           COMPUTE WS-RESULT = 2 + 3 * 4 ** 2.
           IF WS-RESULT = 50
               DISPLAY "NC210A-TEST-1 PASS"
           ELSE
               DISPLAY "NC210A-TEST-1 FAIL"
               DISPLAY "  Expected 50, got " WS-RESULT
           END-IF.
      * Test 2: Parentheses override precedence
      *   COMPUTE R = (2 + 3) * (4 + 1) ** 2
      *   (2+3) = 5, (4+1) = 5, 5 ** 2 = 25, 5 * 25 = 125
           COMPUTE WS-RESULT = (2 + 3) * (4 + 1) ** 2.
           IF WS-RESULT = 125
               DISPLAY "NC210A-TEST-2 PASS"
           ELSE
               DISPLAY "NC210A-TEST-2 FAIL"
               DISPLAY "  Expected 125, got " WS-RESULT
           END-IF.
      * Test 3: Multiple operations with variables (no division)
      *   A=10, B=3, C=2
      *   COMPUTE R = A * B + C ** 3 + A
      *   A*B = 30, C**3 = 8, 30 + 8 + 10 = 48
           MOVE 10 TO WS-A.
           MOVE 3 TO WS-B.
           MOVE 2 TO WS-C.
           COMPUTE WS-RESULT =
               WS-A * WS-B + WS-C ** 3 + WS-A.
           IF WS-RESULT = 48
               DISPLAY "NC210A-TEST-3 PASS"
           ELSE
               DISPLAY "NC210A-TEST-3 FAIL"
               DISPLAY "  Expected 48, got " WS-RESULT
           END-IF.
           STOP RUN.
