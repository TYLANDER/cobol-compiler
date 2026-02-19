       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC173A.
      *
      * NIST CCVS-style test: DIVIDE with REMAINDER
      * Tests the DIVIDE statement with the REMAINDER clause
      * to capture both quotient and remainder.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DIVIDEND      PIC 9(4) VALUE ZEROS.
       01 WS-DIVISOR       PIC 9(4) VALUE ZEROS.
       01 WS-QUOTIENT      PIC 9(4) VALUE ZEROS.
       01 WS-REMAIN        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: 17 / 5 = quotient 3, remainder 2
           MOVE 17 TO WS-DIVIDEND.
           MOVE 5 TO WS-DIVISOR.
           DIVIDE WS-DIVIDEND BY WS-DIVISOR
               GIVING WS-QUOTIENT
               REMAINDER WS-REMAIN.
           IF WS-QUOTIENT = 3 AND WS-REMAIN = 2
               DISPLAY "NC173A-TEST-1 PASS"
           ELSE
               DISPLAY "NC173A-TEST-1 FAIL"
               DISPLAY "  Expected Q=3 R=2, got Q="
                   WS-QUOTIENT " R=" WS-REMAIN
           END-IF.
      * Test 2: 100 / 7 = quotient 14, remainder 2
           MOVE 100 TO WS-DIVIDEND.
           MOVE 7 TO WS-DIVISOR.
           DIVIDE WS-DIVIDEND BY WS-DIVISOR
               GIVING WS-QUOTIENT
               REMAINDER WS-REMAIN.
           IF WS-QUOTIENT = 14 AND WS-REMAIN = 2
               DISPLAY "NC173A-TEST-2 PASS"
           ELSE
               DISPLAY "NC173A-TEST-2 FAIL"
               DISPLAY "  Expected Q=14 R=2, got Q="
                   WS-QUOTIENT " R=" WS-REMAIN
           END-IF.
      * Test 3: Exact division => remainder 0
      *   20 / 4 = quotient 5, remainder 0
           MOVE 20 TO WS-DIVIDEND.
           MOVE 4 TO WS-DIVISOR.
           DIVIDE WS-DIVIDEND BY WS-DIVISOR
               GIVING WS-QUOTIENT
               REMAINDER WS-REMAIN.
           IF WS-QUOTIENT = 5 AND WS-REMAIN = 0
               DISPLAY "NC173A-TEST-3 PASS"
           ELSE
               DISPLAY "NC173A-TEST-3 FAIL"
               DISPLAY "  Expected Q=5 R=0, got Q="
                   WS-QUOTIENT " R=" WS-REMAIN
           END-IF.
           STOP RUN.
