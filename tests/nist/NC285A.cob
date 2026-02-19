       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC285A.
      *
      * NIST CCVS-style test: COMPUTE with complex arithmetic
      * Tests COMPUTE combining addition, subtraction,
      * multiplication, and division with correct precedence.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A             PIC 9(4) VALUE ZEROS.
       01 WS-B             PIC 9(4) VALUE ZEROS.
       01 WS-C             PIC 9(4) VALUE ZEROS.
       01 WS-D             PIC 9(4) VALUE ZEROS.
       01 WS-RESULT        PIC 9(6) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: COMPUTE with all four operations
      *   A=10, B=3, C=4, D=2
      *   RESULT = A * B + C / D - 1
      *   = 10*3 + 4/2 - 1 = 30 + 2 - 1 = 31
           MOVE 10 TO WS-A.
           MOVE 3 TO WS-B.
           MOVE 4 TO WS-C.
           MOVE 2 TO WS-D.
           COMPUTE WS-RESULT =
               WS-A * WS-B + WS-C / WS-D - 1.
           IF WS-RESULT = 31
               DISPLAY "NC285A-TEST-1 PASS"
           ELSE
               DISPLAY "NC285A-TEST-1 FAIL"
               DISPLAY "  Expected 31, got " WS-RESULT
           END-IF.
      * Test 2: COMPUTE with parentheses overriding precedence
      *   A=20, B=5, C=3, D=2
      *   RESULT = (A - B) * (C + D)
      *   = (20 - 5) * (3 + 2) = 15 * 5 = 75
           MOVE 20 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE 3 TO WS-C.
           MOVE 2 TO WS-D.
           COMPUTE WS-RESULT =
               (WS-A - WS-B) * (WS-C + WS-D).
           IF WS-RESULT = 75
               DISPLAY "NC285A-TEST-2 PASS"
           ELSE
               DISPLAY "NC285A-TEST-2 FAIL"
               DISPLAY "  Expected 75, got " WS-RESULT
           END-IF.
      * Test 3: COMPUTE with nested parentheses
      *   A=6, B=2, C=3, D=4
      *   RESULT = ((A + B) * C - D) / D + 1
      *   = ((6+2)*3 - 4) / 4 + 1 = (24-4)/4 + 1 = 20/4 + 1
      *   = 5 + 1 = 6
           MOVE 6 TO WS-A.
           MOVE 2 TO WS-B.
           MOVE 3 TO WS-C.
           MOVE 4 TO WS-D.
           COMPUTE WS-RESULT =
               ((WS-A + WS-B) * WS-C - WS-D)
               / WS-D + 1.
           IF WS-RESULT = 6
               DISPLAY "NC285A-TEST-3 PASS"
           ELSE
               DISPLAY "NC285A-TEST-3 FAIL"
               DISPLAY "  Expected 6, got " WS-RESULT
           END-IF.
           STOP RUN.
