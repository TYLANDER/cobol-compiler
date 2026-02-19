       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC205A.
      *
      * NIST CCVS-style test: Multiple CALLs to same subprogram
      * Tests calling the same subprogram multiple times with
      * different parameter values to verify independent operation.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OP     PIC X(4) VALUE SPACES.
       01 WS-INPUT  PIC 9(4) VALUE 0.
       01 WS-OUTPUT PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: First CALL - ADD operation
           MOVE "ADD " TO WS-OP.
           MOVE 15 TO WS-INPUT.
           MOVE 10 TO WS-OUTPUT.
           CALL "IC205A-SUB" USING WS-OP WS-INPUT WS-OUTPUT.
           IF WS-OUTPUT = 25
               DISPLAY "IC205A-TEST-1 PASS"
           ELSE
               DISPLAY "IC205A-TEST-1 FAIL"
               DISPLAY "  OUTPUT=" WS-OUTPUT
           END-IF.
      * Test 2: Second CALL - SUB operation
           MOVE "SUB " TO WS-OP.
           MOVE 5 TO WS-INPUT.
           MOVE 25 TO WS-OUTPUT.
           CALL "IC205A-SUB" USING WS-OP WS-INPUT WS-OUTPUT.
           IF WS-OUTPUT = 20
               DISPLAY "IC205A-TEST-2 PASS"
           ELSE
               DISPLAY "IC205A-TEST-2 FAIL"
               DISPLAY "  OUTPUT=" WS-OUTPUT
           END-IF.
      * Test 3: Third CALL - MUL operation
           MOVE "MUL " TO WS-OP.
           MOVE 3 TO WS-INPUT.
           MOVE 20 TO WS-OUTPUT.
           CALL "IC205A-SUB" USING WS-OP WS-INPUT WS-OUTPUT.
           IF WS-OUTPUT = 60
               DISPLAY "IC205A-TEST-3 PASS"
           ELSE
               DISPLAY "IC205A-TEST-3 FAIL"
               DISPLAY "  OUTPUT=" WS-OUTPUT
           END-IF.
           STOP RUN.
