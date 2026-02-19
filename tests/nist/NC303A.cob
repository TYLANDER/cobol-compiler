       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC303A.
      *
      * NIST CCVS-style test: PERFORM VARYING with negative step
      * Tests PERFORM VARYING FROM 10 BY -2 UNTIL < 0, verifying
      * iteration count, sum, and final variable value.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I         PIC S9(4) VALUE ZEROS.
       01 WS-COUNT     PIC 9(4) VALUE ZEROS.
       01 WS-SUM       PIC 9(4) VALUE ZEROS.
       01 WS-LAST      PIC S9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC303A-CONTROL.
           PERFORM NC303A-TEST-1.
           PERFORM NC303A-TEST-2.
           PERFORM NC303A-TEST-3.
           STOP RUN.
       NC303A-TEST-1.
      * PERFORM VARYING FROM 10 BY -2 UNTIL < 0
      *   Values: 10, 8, 6, 4, 2, 0 => 6 iterations
      *   Sum = 10+8+6+4+2+0 = 30
           MOVE 0 TO WS-COUNT.
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 10 BY -2
               UNTIL WS-I < 0
               ADD 1 TO WS-COUNT
               ADD WS-I TO WS-SUM
           END-PERFORM.
           IF WS-COUNT = 6 AND WS-SUM = 30
               DISPLAY "NC303A-TEST-1 PASS"
           ELSE
               DISPLAY "NC303A-TEST-1 FAIL"
               DISPLAY "  Expected COUNT=6 SUM=30"
               DISPLAY "  Got COUNT=" WS-COUNT " SUM=" WS-SUM
           END-IF.
       NC303A-TEST-2.
      * PERFORM VARYING FROM 10 BY -3 UNTIL < 0
      *   Values: 10, 7, 4, 1 => 4 iterations
      *   Sum = 10+7+4+1 = 22
           MOVE 0 TO WS-COUNT.
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 10 BY -3
               UNTIL WS-I < 0
               ADD 1 TO WS-COUNT
               ADD WS-I TO WS-SUM
           END-PERFORM.
           IF WS-COUNT = 4 AND WS-SUM = 22
               DISPLAY "NC303A-TEST-2 PASS"
           ELSE
               DISPLAY "NC303A-TEST-2 FAIL"
               DISPLAY "  Expected COUNT=4 SUM=22"
               DISPLAY "  Got COUNT=" WS-COUNT " SUM=" WS-SUM
           END-IF.
       NC303A-TEST-3.
      * PERFORM VARYING FROM 10 BY -2 UNTIL < 0
      *   After loop, WS-I should be negative and LAST = 0
           MOVE 0 TO WS-LAST.
           PERFORM VARYING WS-I FROM 10 BY -2
               UNTIL WS-I < 0
               MOVE WS-I TO WS-LAST
           END-PERFORM.
           IF WS-I < 0 AND WS-LAST = 0
               DISPLAY "NC303A-TEST-3 PASS"
           ELSE
               DISPLAY "NC303A-TEST-3 FAIL"
               DISPLAY "  Expected I<0 LAST=0"
               DISPLAY "  Got I=" WS-I " LAST=" WS-LAST
           END-IF.
