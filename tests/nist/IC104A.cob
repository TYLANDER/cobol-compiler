       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC104A.
      *
      * NIST CCVS-style test: Arithmetic in subprograms
      * Tests CALL passing parameters for computation.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC 9(4) VALUE 25.
       01 WS-B PIC 9(4) VALUE 17.
       01 WS-SUM PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Subprogram computes sum
           CALL "IC104A-SUB" USING WS-A WS-B WS-SUM.
           IF WS-SUM = 42
               DISPLAY "IC104A-TEST-1 PASS"
           ELSE
               DISPLAY "IC104A-TEST-1 FAIL"
               DISPLAY "  SUM=" WS-SUM
           END-IF.
      * Test 2: Different values
           MOVE 100 TO WS-A.
           MOVE 200 TO WS-B.
           CALL "IC104A-SUB" USING WS-A WS-B WS-SUM.
           IF WS-SUM = 300
               DISPLAY "IC104A-TEST-2 PASS"
           ELSE
               DISPLAY "IC104A-TEST-2 FAIL"
               DISPLAY "  SUM=" WS-SUM
           END-IF.
           STOP RUN.
