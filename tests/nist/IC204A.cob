       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC204A.
      *
      * NIST CCVS-style test: CALL with output parameter
      * Tests that a subprogram can compute a value from input
      * parameters and return the result via an output parameter.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INPUT  PIC 9(4) VALUE 0.
       01 WS-RESULT PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Subprogram doubles input, stores in output param
           MOVE 10 TO WS-INPUT.
           MOVE 0 TO WS-RESULT.
           CALL "IC204A-SUB" USING WS-INPUT WS-RESULT.
           IF WS-RESULT = 20
               DISPLAY "IC204A-TEST-1 PASS"
           ELSE
               DISPLAY "IC204A-TEST-1 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
           END-IF.
      * Test 2: Different input value
           MOVE 50 TO WS-INPUT.
           MOVE 0 TO WS-RESULT.
           CALL "IC204A-SUB" USING WS-INPUT WS-RESULT.
           IF WS-RESULT = 100
               DISPLAY "IC204A-TEST-2 PASS"
           ELSE
               DISPLAY "IC204A-TEST-2 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
           END-IF.
      * Test 3: Input parameter is not modified by sub
           IF WS-INPUT = 50
               DISPLAY "IC204A-TEST-3 PASS"
           ELSE
               DISPLAY "IC204A-TEST-3 FAIL"
               DISPLAY "  INPUT=" WS-INPUT
           END-IF.
           STOP RUN.
