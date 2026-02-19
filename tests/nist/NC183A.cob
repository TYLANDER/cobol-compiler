       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC183A.
      *
      * NIST CCVS-style test: COMPUTE with nested parentheses
      * Tests COMPUTE using deeply nested parenthetical expressions
      * to verify correct evaluation order.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A             PIC 9(4) VALUE ZEROS.
       01 WS-B             PIC 9(4) VALUE ZEROS.
       01 WS-C             PIC 9(4) VALUE ZEROS.
       01 WS-D             PIC 9(4) VALUE ZEROS.
       01 WS-E             PIC 9(4) VALUE ZEROS.
       01 WS-RESULT        PIC 9(6) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: COMPUTE A = ((B + C) * D) / E
      *   B=6, C=4, D=5, E=2
      *   = ((6+4)*5)/2 = (10*5)/2 = 50/2 = 25
           MOVE 6 TO WS-B.
           MOVE 4 TO WS-C.
           MOVE 5 TO WS-D.
           MOVE 2 TO WS-E.
           COMPUTE WS-RESULT = ((WS-B + WS-C) * WS-D) / WS-E.
           IF WS-RESULT = 25
               DISPLAY "NC183A-TEST-1 PASS"
           ELSE
               DISPLAY "NC183A-TEST-1 FAIL"
               DISPLAY "  Expected 25, got " WS-RESULT
           END-IF.
      * Test 2: COMPUTE with triple nesting
      *   A = (((B + C) * 2) - D) + E
      *   B=3, C=7, D=8, E=1
      *   = (((3+7)*2)-8)+1 = ((10*2)-8)+1 = (20-8)+1 = 12+1 = 13
           MOVE 3 TO WS-B.
           MOVE 7 TO WS-C.
           MOVE 8 TO WS-D.
           MOVE 1 TO WS-E.
           COMPUTE WS-RESULT =
               (((WS-B + WS-C) * 2) - WS-D) + WS-E.
           IF WS-RESULT = 13
               DISPLAY "NC183A-TEST-2 PASS"
           ELSE
               DISPLAY "NC183A-TEST-2 FAIL"
               DISPLAY "  Expected 13, got " WS-RESULT
           END-IF.
      * Test 3: COMPUTE with adjacent parenthesized groups
      *   A = (B + C) * (D - E)
      *   B=8, C=2, D=9, E=4
      *   = (8+2)*(9-4) = 10*5 = 50
           MOVE 8 TO WS-B.
           MOVE 2 TO WS-C.
           MOVE 9 TO WS-D.
           MOVE 4 TO WS-E.
           COMPUTE WS-RESULT =
               (WS-B + WS-C) * (WS-D - WS-E).
           IF WS-RESULT = 50
               DISPLAY "NC183A-TEST-3 PASS"
           ELSE
               DISPLAY "NC183A-TEST-3 FAIL"
               DISPLAY "  Expected 50, got " WS-RESULT
           END-IF.
           STOP RUN.
