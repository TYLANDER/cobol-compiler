       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC136A.
      *
      * NIST CCVS-style test: DIVIDE BY variations
      * Complements NC128A which tests DIVIDE INTO.
      * Tests DIVIDE BY GIVING and DIVIDE BY GIVING REMAINDER.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A           PIC 9(4) VALUE ZEROS.
       01 WS-B           PIC 9(4) VALUE ZEROS.
       01 WS-C           PIC 9(4) VALUE ZEROS.
       01 WS-D           PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: DIVIDE A BY B GIVING C
      *   C = A / B
           MOVE 20 TO WS-A.
           MOVE 4 TO WS-B.
           MOVE 0 TO WS-C.
           DIVIDE WS-A BY WS-B GIVING WS-C.
           IF WS-C = 5
               DISPLAY "NC136A-TEST-1 PASS"
           ELSE
               DISPLAY "NC136A-TEST-1 FAIL"
               DISPLAY "  Expected C=5, got " WS-C
           END-IF.
      * Test 2: DIVIDE A BY B GIVING C REMAINDER D
      *   C = integer part of A / B, D = remainder
           MOVE 17 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE 0 TO WS-C.
           MOVE 0 TO WS-D.
           DIVIDE WS-A BY WS-B
               GIVING WS-C REMAINDER WS-D.
           IF WS-C = 3 AND WS-D = 2
               DISPLAY "NC136A-TEST-2 PASS"
           ELSE
               DISPLAY "NC136A-TEST-2 FAIL"
               DISPLAY "  Expected C=3 D=2, got C="
                   WS-C " D=" WS-D
           END-IF.
           STOP RUN.
