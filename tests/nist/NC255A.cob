       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC255A.
      *
      * NIST CCVS-style test: PERFORM VARYING ... AFTER
      * Tests nested loop with two varying counters.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I        PIC 9(4) VALUE ZEROS.
       01 WS-J        PIC 9(4) VALUE ZEROS.
       01 WS-COUNT    PIC 9(4) VALUE ZEROS.
       01 WS-SUM      PIC 9(8) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC255A-CONTROL.
           PERFORM NC255A-TEST-1.
           PERFORM NC255A-TEST-2.
           PERFORM NC255A-TEST-3.
           STOP RUN.
       NC255A-TEST-1.
      * PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
      *     AFTER J FROM 1 BY 1 UNTIL J > 4
      * Total iterations = 3 * 4 = 12
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               AFTER WS-J FROM 1 BY 1
               UNTIL WS-J > 4
               ADD 1 TO WS-COUNT
           END-PERFORM.
           IF WS-COUNT = 12
               DISPLAY "NC255A-TEST-1 PASS"
           ELSE
               DISPLAY "NC255A-TEST-1 FAIL"
               DISPLAY "  Expected 12, got " WS-COUNT
           END-IF.
       NC255A-TEST-2.
      * Sum of I*J for I=1..2, J=1..3
      * (1*1+1*2+1*3)+(2*1+2*2+2*3) = 6+12 = 18
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 2
               AFTER WS-J FROM 1 BY 1
               UNTIL WS-J > 3
               COMPUTE WS-SUM = WS-SUM + WS-I * WS-J
           END-PERFORM.
           IF WS-SUM = 18
               DISPLAY "NC255A-TEST-2 PASS"
           ELSE
               DISPLAY "NC255A-TEST-2 FAIL"
               DISPLAY "  Expected 18, got " WS-SUM
           END-IF.
       NC255A-TEST-3.
      * Verify final values of counters after loop
      * After VARYING I FROM 1 BY 1 UNTIL I > 2
      *     AFTER J FROM 1 BY 1 UNTIL J > 3
      * I should be 3, J should be 4 (one past the limit)
           MOVE 0 TO WS-I.
           MOVE 0 TO WS-J.
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 2
               AFTER WS-J FROM 1 BY 1
               UNTIL WS-J > 3
               ADD 1 TO WS-COUNT
           END-PERFORM.
           IF WS-I = 3 AND WS-J = 4
               DISPLAY "NC255A-TEST-3 PASS"
           ELSE
               DISPLAY "NC255A-TEST-3 FAIL"
               DISPLAY "  Expected I=3 J=4, got I="
                   WS-I " J=" WS-J
           END-IF.
