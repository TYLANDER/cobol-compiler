       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC191A.
      *
      * NIST CCVS-style test: COMPUTE with complex nested parentheses
      * Tests deeply nested parenthesized arithmetic expressions
      * to verify correct operator precedence and grouping.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A          PIC 9(4) VALUE ZEROS.
       01 WS-B          PIC 9(4) VALUE ZEROS.
       01 WS-C          PIC 9(4) VALUE ZEROS.
       01 WS-D          PIC 9(4) VALUE ZEROS.
       01 WS-E          PIC 9(4) VALUE ZEROS.
       01 WS-RESULT     PIC 9(8) VALUE ZEROS.
       01 WS-DEC-RES    PIC 9(5)V99 VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: COMPUTE X = ((A + B) * (C - D)) / E
      *   A=10, B=20, C=15, D=5, E=6
      *   ((10 + 20) * (15 - 5)) / 6 = (30 * 10) / 6 = 300 / 6 = 50
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 15 TO WS-C.
           MOVE 5 TO WS-D.
           MOVE 6 TO WS-E.
           COMPUTE WS-RESULT =
               ((WS-A + WS-B) * (WS-C - WS-D)) / WS-E.
           IF WS-RESULT = 50
               DISPLAY "NC191A-TEST-1 PASS"
           ELSE
               DISPLAY "NC191A-TEST-1 FAIL"
               DISPLAY "  Expected 50, got " WS-RESULT
           END-IF.
      * Test 2: COMPUTE X = (A * (B + C)) - (D * E)
      *   A=3, B=4, C=6, D=2, E=5
      *   (3 * (4 + 6)) - (2 * 5) = (3 * 10) - 10 = 30 - 10 = 20
           MOVE 3 TO WS-A.
           MOVE 4 TO WS-B.
           MOVE 6 TO WS-C.
           MOVE 2 TO WS-D.
           MOVE 5 TO WS-E.
           COMPUTE WS-RESULT =
               (WS-A * (WS-B + WS-C)) - (WS-D * WS-E).
           IF WS-RESULT = 20
               DISPLAY "NC191A-TEST-2 PASS"
           ELSE
               DISPLAY "NC191A-TEST-2 FAIL"
               DISPLAY "  Expected 20, got " WS-RESULT
           END-IF.
      * Test 3: COMPUTE X = ((A + B) * (C + D)) / ((A - B) + E)
      *   A=8, B=2, C=3, D=7, E=4
      *   ((8 + 2) * (3 + 7)) / ((8 - 2) + 4)
      *   = (10 * 10) / (6 + 4) = 100 / 10 = 10
           MOVE 8 TO WS-A.
           MOVE 2 TO WS-B.
           MOVE 3 TO WS-C.
           MOVE 7 TO WS-D.
           MOVE 4 TO WS-E.
           COMPUTE WS-RESULT =
               ((WS-A + WS-B) * (WS-C + WS-D))
               / ((WS-A - WS-B) + WS-E).
           IF WS-RESULT = 10
               DISPLAY "NC191A-TEST-3 PASS"
           ELSE
               DISPLAY "NC191A-TEST-3 FAIL"
               DISPLAY "  Expected 10, got " WS-RESULT
           END-IF.
           STOP RUN.
