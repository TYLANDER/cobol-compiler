       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC106A.
      *
      * NIST CCVS-style test: Multiple CALLs to same subprogram
      * Tests calling the same subprogram twice in succession,
      * verifying cumulative effects on a numeric parameter.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: First call adds 10 (0 -> 10)
           CALL "IC106A-SUB" USING WS-COUNTER.
           IF WS-COUNTER = 10
               DISPLAY "IC106A-TEST-1 PASS"
           ELSE
               DISPLAY "IC106A-TEST-1 FAIL"
               DISPLAY "  COUNTER=" WS-COUNTER
           END-IF.
      * Test 2: Second call adds 10 again (10 -> 20)
           CALL "IC106A-SUB" USING WS-COUNTER.
           IF WS-COUNTER = 20
               DISPLAY "IC106A-TEST-2 PASS"
           ELSE
               DISPLAY "IC106A-TEST-2 FAIL"
               DISPLAY "  COUNTER=" WS-COUNTER
           END-IF.
           STOP RUN.
