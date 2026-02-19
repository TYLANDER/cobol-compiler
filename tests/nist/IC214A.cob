       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC214A.
      *
      * NIST CCVS-style test: chained CALL (sub calls another sub).
      * Main calls IC214A-SUB which calls IC214A-SUB2, creating a
      * two-level call chain. Verifies data flows through the chain.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VALUE PIC 9(4) VALUE 0.
       01 WS-TRACE PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Chained call modifies numeric through both levels
           MOVE 10 TO WS-VALUE.
           MOVE SPACES TO WS-TRACE.
           CALL "IC214A-SUB" USING WS-VALUE WS-TRACE.
           IF WS-VALUE = 60
               DISPLAY "IC214A-TEST-1 PASS"
           ELSE
               DISPLAY "IC214A-TEST-1 FAIL"
               DISPLAY "  VALUE=" WS-VALUE
           END-IF.
      * Test 2: Trace confirms both subprograms executed
           IF WS-TRACE = "LEVEL1-LEVEL2       "
               DISPLAY "IC214A-TEST-2 PASS"
           ELSE
               DISPLAY "IC214A-TEST-2 FAIL"
               DISPLAY "  TRACE=>" WS-TRACE "<"
           END-IF.
      * Test 3: Second call through chain with different input
           MOVE 5 TO WS-VALUE.
           MOVE SPACES TO WS-TRACE.
           CALL "IC214A-SUB" USING WS-VALUE WS-TRACE.
           IF WS-VALUE = 30 AND WS-TRACE = "LEVEL1-LEVEL2       "
               DISPLAY "IC214A-TEST-3 PASS"
           ELSE
               DISPLAY "IC214A-TEST-3 FAIL"
               DISPLAY "  VALUE=" WS-VALUE
               DISPLAY "  TRACE=>" WS-TRACE "<"
           END-IF.
           STOP RUN.
