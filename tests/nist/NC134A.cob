       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC134A.
      *
      * NIST CCVS-style test: SUBTRACT statement variations
      * Tests SUBTRACT FROM, SUBTRACT FROM GIVING, and
      * SUBTRACT with multiple operands.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A           PIC 9(4) VALUE ZEROS.
       01 WS-B           PIC 9(4) VALUE ZEROS.
       01 WS-C           PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: SUBTRACT A FROM B (B = B - A)
           MOVE 3 TO WS-A.
           MOVE 10 TO WS-B.
           SUBTRACT WS-A FROM WS-B.
           IF WS-B = 7
               DISPLAY "NC134A-TEST-1 PASS"
           ELSE
               DISPLAY "NC134A-TEST-1 FAIL"
               DISPLAY "  Expected B=7, got " WS-B
           END-IF.
      * Test 2: SUBTRACT A FROM B GIVING C
      *   C = B - A, B is unchanged
           MOVE 5 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 0 TO WS-C.
           SUBTRACT WS-A FROM WS-B GIVING WS-C.
           IF WS-C = 15
               DISPLAY "NC134A-TEST-2 PASS"
           ELSE
               DISPLAY "NC134A-TEST-2 FAIL"
               DISPLAY "  Expected C=15, got " WS-C
           END-IF.
      * Test 3: SUBTRACT A B FROM C (multiple operands)
      *   C = C - A - B
           MOVE 2 TO WS-A.
           MOVE 3 TO WS-B.
           MOVE 20 TO WS-C.
           SUBTRACT WS-A WS-B FROM WS-C.
           IF WS-C = 15
               DISPLAY "NC134A-TEST-3 PASS"
           ELSE
               DISPLAY "NC134A-TEST-3 FAIL"
               DISPLAY "  Expected C=15, got " WS-C
           END-IF.
           STOP RUN.
