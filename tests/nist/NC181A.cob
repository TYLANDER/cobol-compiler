       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC181A.
      *
      * NIST CCVS-style test: PERFORM THRU with TIMES
      * Tests out-of-line PERFORM with THRU and TIMES clause
      * to verify paragraph range execution and iteration count.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER        PIC 9(4) VALUE ZEROS.
       01 WS-ACCUM          PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: PERFORM paragraph THRU paragraph 3 TIMES
      *   Each execution of PARA-A THRU PARA-B adds 10 then 5
      *   3 iterations => COUNTER should be 45
           MOVE 0 TO WS-COUNTER.
           PERFORM PARA-A THRU PARA-B 3 TIMES.
           IF WS-COUNTER = 45
               DISPLAY "NC181A-TEST-1 PASS"
           ELSE
               DISPLAY "NC181A-TEST-1 FAIL"
               DISPLAY "  Expected 45, got " WS-COUNTER
           END-IF.
      * Test 2: PERFORM THRU 1 TIMES (single iteration)
      *   One pass through PARA-C THRU PARA-D adds 100
           MOVE 0 TO WS-ACCUM.
           PERFORM PARA-C THRU PARA-D 1 TIMES.
           IF WS-ACCUM = 100
               DISPLAY "NC181A-TEST-2 PASS"
           ELSE
               DISPLAY "NC181A-TEST-2 FAIL"
               DISPLAY "  Expected 100, got " WS-ACCUM
           END-IF.
      * Test 3: PERFORM THRU 0 TIMES (zero iterations)
      *   Zero iterations => ACCUM should remain unchanged
           MOVE 0 TO WS-ACCUM.
           PERFORM PARA-C THRU PARA-D 0 TIMES.
           IF WS-ACCUM = 0
               DISPLAY "NC181A-TEST-3 PASS"
           ELSE
               DISPLAY "NC181A-TEST-3 FAIL"
               DISPLAY "  Expected 0, got " WS-ACCUM
           END-IF.
           STOP RUN.
       PARA-A.
           ADD 10 TO WS-COUNTER.
       PARA-B.
           ADD 5 TO WS-COUNTER.
       PARA-C.
           ADD 75 TO WS-ACCUM.
       PARA-D.
           ADD 25 TO WS-ACCUM.
