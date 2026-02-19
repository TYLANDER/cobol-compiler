       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC222A.
      *
      * NIST CCVS-style test: Numeric comparison edge cases
      * Tests comparing values stored in fields with different
      * PIC sizes (9 vs 9(4)), and zero comparisons.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SMALL         PIC 9    VALUE 5.
       01 WS-LARGE         PIC 9(4) VALUE 5.
       01 WS-BIG           PIC 9(4) VALUE 9999.
       01 WS-ZERO-S        PIC 9    VALUE 0.
       01 WS-ZERO-L        PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Compare PIC 9 to PIC 9(4), same value
      *   Both hold 5, should be equal
           IF WS-SMALL = WS-LARGE
               DISPLAY "NC222A-TEST-1 PASS"
           ELSE
               DISPLAY "NC222A-TEST-1 FAIL"
               DISPLAY "  SMALL=" WS-SMALL " LARGE=" WS-LARGE
           END-IF.
      * Test 2: Compare PIC 9 with literal > its range
      *   WS-SMALL (PIC 9, val=5) < 9999
           IF WS-SMALL < WS-BIG
               DISPLAY "NC222A-TEST-2 PASS"
           ELSE
               DISPLAY "NC222A-TEST-2 FAIL"
               DISPLAY "  SMALL=" WS-SMALL " BIG=" WS-BIG
           END-IF.
      * Test 3: Compare zero in different PIC sizes
      *   PIC 9 zero and PIC 9(4) zero should be equal
           IF WS-ZERO-S = WS-ZERO-L
               AND WS-ZERO-S = 0
               AND WS-ZERO-L = ZEROS
               DISPLAY "NC222A-TEST-3 PASS"
           ELSE
               DISPLAY "NC222A-TEST-3 FAIL"
               DISPLAY "  ZERO-S=" WS-ZERO-S
                   " ZERO-L=" WS-ZERO-L
           END-IF.
           STOP RUN.
