       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC209A.
      *
      * NIST CCVS-style test: Multiple CALLs with different data
      * Tests calling the same subprogram multiple times with
      * different parameter values to verify independent operation.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ACTION PIC X(4) VALUE SPACES.
       01 WS-INPUT  PIC 9(4) VALUE 0.
       01 WS-RESULT PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: First CALL - ADD operation
           MOVE "ADD " TO WS-ACTION.
           MOVE 100 TO WS-INPUT.
           MOVE 200 TO WS-RESULT.
           CALL "IC209A-SUB" USING WS-ACTION WS-INPUT WS-RESULT.
           IF WS-RESULT = 300
               DISPLAY "IC209A-TEST-1 PASS"
           ELSE
               DISPLAY "IC209A-TEST-1 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
           END-IF.
      * Test 2: Second CALL - SUB operation with new data
           MOVE "SUB " TO WS-ACTION.
           MOVE 50 TO WS-INPUT.
           MOVE 300 TO WS-RESULT.
           CALL "IC209A-SUB" USING WS-ACTION WS-INPUT WS-RESULT.
           IF WS-RESULT = 250
               DISPLAY "IC209A-TEST-2 PASS"
           ELSE
               DISPLAY "IC209A-TEST-2 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
           END-IF.
      * Test 3: Third CALL - MUL operation with new data
           MOVE "MUL " TO WS-ACTION.
           MOVE 4 TO WS-INPUT.
           MOVE 250 TO WS-RESULT.
           CALL "IC209A-SUB" USING WS-ACTION WS-INPUT WS-RESULT.
           IF WS-RESULT = 1000
               DISPLAY "IC209A-TEST-3 PASS"
           ELSE
               DISPLAY "IC209A-TEST-3 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
           END-IF.
           STOP RUN.
