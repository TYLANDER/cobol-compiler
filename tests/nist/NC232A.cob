       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC232A.
      *
      * NIST CCVS-style test: DIVIDE with REMAINDER clause
      * Tests DIVIDE GIVING REMAINDER to verify quotient
      * and remainder values.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DIVIDEND  PIC 9(4) VALUE ZEROS.
       01 WS-DIVISOR   PIC 9(4) VALUE ZEROS.
       01 WS-QUOTIENT  PIC 9(4) VALUE ZEROS.
       01 WS-REMAINDER PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: DIVIDE A BY B GIVING C REMAINDER D
      *   17 / 5 = 3 remainder 2
           MOVE 17 TO WS-DIVIDEND.
           MOVE 5 TO WS-DIVISOR.
           DIVIDE WS-DIVIDEND BY WS-DIVISOR
               GIVING WS-QUOTIENT REMAINDER WS-REMAINDER.
           IF WS-QUOTIENT = 3 AND WS-REMAINDER = 2
               DISPLAY "NC232A-TEST-1 PASS"
           ELSE
               DISPLAY "NC232A-TEST-1 FAIL"
               DISPLAY "  Expected Q=3 R=2, got Q="
                   WS-QUOTIENT " R=" WS-REMAINDER
           END-IF.
      * Test 2: Verify quotient is correct
      *   100 / 7 = 14 remainder 2
           MOVE 100 TO WS-DIVIDEND.
           MOVE 7 TO WS-DIVISOR.
           DIVIDE WS-DIVIDEND BY WS-DIVISOR
               GIVING WS-QUOTIENT REMAINDER WS-REMAINDER.
           IF WS-QUOTIENT = 14
               DISPLAY "NC232A-TEST-2 PASS"
           ELSE
               DISPLAY "NC232A-TEST-2 FAIL"
               DISPLAY "  Expected Q=14, got Q=" WS-QUOTIENT
           END-IF.
      * Test 3: Verify remainder is correct
      *   100 / 7 = 14 remainder 2 (continued from above)
           IF WS-REMAINDER = 2
               DISPLAY "NC232A-TEST-3 PASS"
           ELSE
               DISPLAY "NC232A-TEST-3 FAIL"
               DISPLAY "  Expected R=2, got R=" WS-REMAINDER
           END-IF.
           STOP RUN.
