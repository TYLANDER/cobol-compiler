       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC149A.
      *
      * NIST CCVS-style test: COMPUTE with complex expressions
      * Tests COMPUTE using parentheses, multiple operators,
      * and order of operations.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(4) VALUE ZEROS.
       01 WS-B            PIC 9(4) VALUE ZEROS.
       01 WS-C            PIC 9(4) VALUE ZEROS.
       01 WS-RESULT       PIC 9(4) VALUE ZEROS.
       01 WS-RESULT2      PIC 9(6) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: COMPUTE with addition and multiplication
      *   RESULT = A + B * C
      *   A=2, B=3, C=4 => 2 + 3*4 = 2 + 12 = 14
           MOVE 2 TO WS-A.
           MOVE 3 TO WS-B.
           MOVE 4 TO WS-C.
           COMPUTE WS-RESULT = WS-A + WS-B * WS-C.
           IF WS-RESULT = 14
               DISPLAY "NC149A-TEST-1 PASS"
           ELSE
               DISPLAY "NC149A-TEST-1 FAIL"
               DISPLAY "  Expected 14, got " WS-RESULT
           END-IF.
      * Test 2: COMPUTE with parentheses overriding precedence
      *   RESULT = (A + B) * C
      *   A=2, B=3, C=4 => (2+3)*4 = 5*4 = 20
           MOVE 2 TO WS-A.
           MOVE 3 TO WS-B.
           MOVE 4 TO WS-C.
           COMPUTE WS-RESULT = (WS-A + WS-B) * WS-C.
           IF WS-RESULT = 20
               DISPLAY "NC149A-TEST-2 PASS"
           ELSE
               DISPLAY "NC149A-TEST-2 FAIL"
               DISPLAY "  Expected 20, got " WS-RESULT
           END-IF.
      * Test 3: COMPUTE with nested parentheses and subtraction
      *   RESULT = (A * (B + C)) - A
      *   A=5, B=3, C=7 => (5 * (3+7)) - 5 = (5*10) - 5 = 45
           MOVE 5 TO WS-A.
           MOVE 3 TO WS-B.
           MOVE 7 TO WS-C.
           COMPUTE WS-RESULT = (WS-A * (WS-B + WS-C)) - WS-A.
           IF WS-RESULT = 45
               DISPLAY "NC149A-TEST-3 PASS"
           ELSE
               DISPLAY "NC149A-TEST-3 FAIL"
               DISPLAY "  Expected 45, got " WS-RESULT
           END-IF.
           STOP RUN.
