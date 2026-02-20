       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC366A.
      *
      * NIST CCVS-style test: DIVIDE with REMAINDER
      * Tests integer division with remainder on DISPLAY fields.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-P PIC 9(4) VALUE 0.
       01 WS-Q PIC 9(4) VALUE 0.
       01 WS-R PIC 9(4) VALUE 0.
       01 WS-D PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: 100 / 7 = 14 remainder 2
           MOVE 100 TO WS-P.
           MOVE 7 TO WS-Q.
           DIVIDE WS-P BY WS-Q GIVING WS-R
               REMAINDER WS-D.
           IF WS-R = 14 AND WS-D = 2
               DISPLAY "NC366A-TEST-1 PASS"
           ELSE
               DISPLAY "NC366A-TEST-1 FAIL"
               DISPLAY "  Expected 14 rem 2"
               DISPLAY "  Got " WS-R " rem " WS-D
           END-IF.
      * Test 2: 999 / 10 = 99 remainder 9
           MOVE 999 TO WS-P.
           MOVE 10 TO WS-Q.
           DIVIDE WS-P BY WS-Q GIVING WS-R
               REMAINDER WS-D.
           IF WS-R = 99 AND WS-D = 9
               DISPLAY "NC366A-TEST-2 PASS"
           ELSE
               DISPLAY "NC366A-TEST-2 FAIL"
               DISPLAY "  Expected 99 rem 9"
               DISPLAY "  Got " WS-R " rem " WS-D
           END-IF.
      * Test 3: 50 / 50 = 1 remainder 0
           MOVE 50 TO WS-P.
           MOVE 50 TO WS-Q.
           DIVIDE WS-P BY WS-Q GIVING WS-R
               REMAINDER WS-D.
           IF WS-R = 1 AND WS-D = 0
               DISPLAY "NC366A-TEST-3 PASS"
           ELSE
               DISPLAY "NC366A-TEST-3 FAIL"
               DISPLAY "  Expected 1 rem 0"
               DISPLAY "  Got " WS-R " rem " WS-D
           END-IF.
           STOP RUN.
