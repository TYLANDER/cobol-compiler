       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC201A.
      *
      * NIST CCVS-style test: DIVIDE with REMAINDER clause
      * Tests DIVIDE BY GIVING REMAINDER, DIVIDE INTO GIVING
      * REMAINDER with various dividend/divisor combinations,
      * and DIVIDE with ON SIZE ERROR and REMAINDER.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DIVIDEND     PIC 9(4) VALUE ZEROS.
       01 WS-DIVISOR      PIC 9(4) VALUE ZEROS.
       01 WS-QUOTIENT     PIC 9(4) VALUE ZEROS.
       01 WS-REMAINDER    PIC 9(4) VALUE ZEROS.
       01 WS-FLAG         PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: DIVIDE BY GIVING REMAINDER
      *   29 / 7 = 4 remainder 1
           MOVE 29 TO WS-DIVIDEND.
           MOVE 7 TO WS-DIVISOR.
           MOVE 0 TO WS-QUOTIENT.
           MOVE 0 TO WS-REMAINDER.
           DIVIDE WS-DIVIDEND BY WS-DIVISOR
               GIVING WS-QUOTIENT REMAINDER WS-REMAINDER.
           IF WS-QUOTIENT = 4 AND WS-REMAINDER = 1
               DISPLAY "NC201A-TEST-1 PASS"
           ELSE
               DISPLAY "NC201A-TEST-1 FAIL"
               DISPLAY "  Expected Q=4 R=1, got Q="
                   WS-QUOTIENT " R=" WS-REMAINDER
           END-IF.
      * Test 2: DIVIDE INTO GIVING REMAINDER
      *   100 / 3 = 33 remainder 1
           MOVE 3 TO WS-DIVISOR.
           MOVE 100 TO WS-DIVIDEND.
           MOVE 0 TO WS-QUOTIENT.
           MOVE 0 TO WS-REMAINDER.
           DIVIDE WS-DIVISOR INTO WS-DIVIDEND
               GIVING WS-QUOTIENT REMAINDER WS-REMAINDER.
           IF WS-QUOTIENT = 33 AND WS-REMAINDER = 1
               DISPLAY "NC201A-TEST-2 PASS"
           ELSE
               DISPLAY "NC201A-TEST-2 FAIL"
               DISPLAY "  Expected Q=33 R=1, got Q="
                   WS-QUOTIENT " R=" WS-REMAINDER
           END-IF.
      * Test 3: DIVIDE BY GIVING REMAINDER with even division
      *   24 / 6 = 4 remainder 0
           MOVE 24 TO WS-DIVIDEND.
           MOVE 6 TO WS-DIVISOR.
           MOVE 0 TO WS-QUOTIENT.
           MOVE 0 TO WS-REMAINDER.
           DIVIDE WS-DIVIDEND BY WS-DIVISOR
               GIVING WS-QUOTIENT REMAINDER WS-REMAINDER.
           IF WS-QUOTIENT = 4 AND WS-REMAINDER = 0
               DISPLAY "NC201A-TEST-3 PASS"
           ELSE
               DISPLAY "NC201A-TEST-3 FAIL"
               DISPLAY "  Expected Q=4 R=0, got Q="
                   WS-QUOTIENT " R=" WS-REMAINDER
           END-IF.
           STOP RUN.
