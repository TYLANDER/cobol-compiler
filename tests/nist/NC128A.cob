       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC128A.
      *
      * NIST CCVS-style test: DIVIDE statement variations
      * Tests DIVIDE INTO, DIVIDE INTO GIVING, and
      * DIVIDE INTO GIVING REMAINDER.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A           PIC 9(4) VALUE ZEROS.
       01 WS-B           PIC 9(4) VALUE ZEROS.
       01 WS-C           PIC 9(4) VALUE ZEROS.
       01 WS-D           PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: DIVIDE A INTO B (B is modified to B / A)
           MOVE 3 TO WS-A.
           MOVE 12 TO WS-B.
           DIVIDE WS-A INTO WS-B.
           IF WS-B = 4
               DISPLAY "NC128A-TEST-1 PASS"
           ELSE
               DISPLAY "NC128A-TEST-1 FAIL"
               DISPLAY "  Expected B=4, got " WS-B
           END-IF.
      * Test 2: DIVIDE A INTO B GIVING C
      *   C = B / A, B is unchanged
           MOVE 5 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 0 TO WS-C.
           DIVIDE WS-A INTO WS-B GIVING WS-C.
           IF WS-C = 4
               DISPLAY "NC128A-TEST-2 PASS"
           ELSE
               DISPLAY "NC128A-TEST-2 FAIL"
               DISPLAY "  Expected C=4, got " WS-C
           END-IF.
      * Test 3: DIVIDE A INTO B GIVING C REMAINDER D
      *   C = integer part of B / A, D = remainder
           MOVE 3 TO WS-A.
           MOVE 17 TO WS-B.
           MOVE 0 TO WS-C.
           MOVE 0 TO WS-D.
           DIVIDE WS-A INTO WS-B
               GIVING WS-C REMAINDER WS-D.
           IF WS-C = 5 AND WS-D = 2
               DISPLAY "NC128A-TEST-3 PASS"
           ELSE
               DISPLAY "NC128A-TEST-3 FAIL"
               DISPLAY "  Expected C=5 D=2, got C="
                   WS-C " D=" WS-D
           END-IF.
           STOP RUN.
