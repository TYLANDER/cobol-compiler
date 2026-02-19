       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC292A.
      *
      * NIST CCVS-style test: Signed numeric arithmetic
      * Tests COMPUTE and arithmetic with signed fields,
      * negative results, and sign preservation.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A           PIC S9(4) VALUE ZEROS.
       01 WS-B           PIC S9(4) VALUE ZEROS.
       01 WS-RESULT      PIC S9(4) VALUE ZEROS.
       01 WS-EXPECTED     PIC S9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: COMPUTE producing a negative result
      *   10 - 25 = -15
           MOVE 10 TO WS-A.
           MOVE 25 TO WS-B.
           COMPUTE WS-RESULT = WS-A - WS-B.
           COMPUTE WS-EXPECTED = 0 - 15.
           IF WS-RESULT = WS-EXPECTED
               DISPLAY "NC292A-TEST-1 PASS"
           ELSE
               DISPLAY "NC292A-TEST-1 FAIL"
               DISPLAY "  Expected -15, got " WS-RESULT
           END-IF.
      * Test 2: Negative value in SUBTRACT
      *   SUBTRACT 100 FROM 30 => 30 - 100 = -70
           MOVE 30 TO WS-RESULT.
           SUBTRACT 100 FROM WS-RESULT.
           COMPUTE WS-EXPECTED = 0 - 70.
           IF WS-RESULT = WS-EXPECTED
               DISPLAY "NC292A-TEST-2 PASS"
           ELSE
               DISPLAY "NC292A-TEST-2 FAIL"
               DISPLAY "  Expected -70, got " WS-RESULT
           END-IF.
      * Test 3: Negative then add back to positive
      *   Start with -50, add 80 => 30
           COMPUTE WS-RESULT = 0 - 50.
           ADD 80 TO WS-RESULT.
           IF WS-RESULT = 30
               DISPLAY "NC292A-TEST-3 PASS"
           ELSE
               DISPLAY "NC292A-TEST-3 FAIL"
               DISPLAY "  Expected 30, got " WS-RESULT
           END-IF.
           STOP RUN.
