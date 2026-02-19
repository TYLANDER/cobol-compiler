       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC153A.
      *
      * NIST CCVS-style test: PERFORM VARYING with negative step
      * Tests PERFORM VARYING with BY -1 to count down,
      * verifying iteration count and final value.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I             PIC S9(4) VALUE ZEROS.
       01 WS-COUNT         PIC 9(4) VALUE ZEROS.
       01 WS-SUM           PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: PERFORM VARYING counting down from 5 to 1
      *   I goes 5, 4, 3, 2, 1 => 5 iterations
      *   SUM = 5+4+3+2+1 = 15
           MOVE 0 TO WS-COUNT.
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 5 BY -1
               UNTIL WS-I < 1
               ADD 1 TO WS-COUNT
               ADD WS-I TO WS-SUM
           END-PERFORM.
           IF WS-COUNT = 5 AND WS-SUM = 15
               DISPLAY "NC153A-TEST-1 PASS"
           ELSE
               DISPLAY "NC153A-TEST-1 FAIL"
               DISPLAY "  Expected COUNT=5 SUM=15"
               DISPLAY "  Got COUNT=" WS-COUNT " SUM=" WS-SUM
           END-IF.
      * Test 2: PERFORM VARYING counting down from 10 to 1 by -2
      *   I goes 10, 8, 6, 4, 2 => 5 iterations
      *   SUM = 10+8+6+4+2 = 30
           MOVE 0 TO WS-COUNT.
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 10 BY -2
               UNTIL WS-I < 1
               ADD 1 TO WS-COUNT
               ADD WS-I TO WS-SUM
           END-PERFORM.
           IF WS-COUNT = 5 AND WS-SUM = 30
               DISPLAY "NC153A-TEST-2 PASS"
           ELSE
               DISPLAY "NC153A-TEST-2 FAIL"
               DISPLAY "  Expected COUNT=5 SUM=30"
               DISPLAY "  Got COUNT=" WS-COUNT " SUM=" WS-SUM
           END-IF.
           STOP RUN.
