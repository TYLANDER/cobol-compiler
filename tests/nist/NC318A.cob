       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC318A.
      *
      * NIST CCVS-style test: COMPUTE with negative results in S9
      * Tests that COMPUTE can produce negative results and store
      * them correctly in PIC S9 signed numeric fields.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC S9(4) VALUE ZEROS.
       01 WS-B            PIC S9(4) VALUE ZEROS.
       01 WS-RESULT       PIC S9(4) VALUE ZEROS.
       01 WS-EXPECTED     PIC S9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: COMPUTE producing negative result
      *   A=10 B=25 => RESULT = 10 - 25 = -15
      *   EXPECTED = 0 - 15 = -15
           MOVE 10 TO WS-A.
           MOVE 25 TO WS-B.
           COMPUTE WS-RESULT = WS-A - WS-B.
           COMPUTE WS-EXPECTED = 0 - 15.
           IF WS-RESULT = WS-EXPECTED
               DISPLAY "NC318A-TEST-1 PASS"
           ELSE
               DISPLAY "NC318A-TEST-1 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
               DISPLAY "  EXPECTED=" WS-EXPECTED
           END-IF.
      * Test 2: COMPUTE negative then add back to positive
      *   RESULT = 0 - 50 = -50, then add 80 => 30
           COMPUTE WS-RESULT = 0 - 50.
           ADD 80 TO WS-RESULT.
           IF WS-RESULT = 30
               DISPLAY "NC318A-TEST-2 PASS"
           ELSE
               DISPLAY "NC318A-TEST-2 FAIL"
               DISPLAY "  Expected 30, got " WS-RESULT
           END-IF.
      * Test 3: SUBTRACT yielding negative, compare with COMPUTE
      *   RESULT = 20, SUBTRACT 35 => -15
      *   EXPECTED via COMPUTE = 0 - 15 = -15
           MOVE 20 TO WS-RESULT.
           SUBTRACT 35 FROM WS-RESULT.
           COMPUTE WS-EXPECTED = 0 - 15.
           IF WS-RESULT = WS-EXPECTED
               DISPLAY "NC318A-TEST-3 PASS"
           ELSE
               DISPLAY "NC318A-TEST-3 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
               DISPLAY "  EXPECTED=" WS-EXPECTED
           END-IF.
           STOP RUN.
