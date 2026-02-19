       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC272A.
      *
      * NIST CCVS-style test: Nested PERFORM VARYING
      * Tests PERFORM VARYING with AFTER clause creating
      * nested loops, verifying iteration counts and sums.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I          PIC 9(4) VALUE ZEROS.
       01 WS-J          PIC 9(4) VALUE ZEROS.
       01 WS-COUNT      PIC 9(4) VALUE ZEROS.
       01 WS-SUM        PIC 9(8) VALUE ZEROS.
       01 WS-LAST-I     PIC 9(4) VALUE ZEROS.
       01 WS-LAST-J     PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC272A-CONTROL.
           PERFORM NC272A-TEST-1.
           PERFORM NC272A-TEST-2.
           PERFORM NC272A-TEST-3.
           STOP RUN.
       NC272A-TEST-1.
      * PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
      *     AFTER J FROM 1 BY 1 UNTIL J > 4
      * Outer: I=1,2,3 (3 values)
      * Inner: J=1,2,3,4 (4 values per I)
      * Total iterations = 3 * 4 = 12
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               AFTER WS-J FROM 1 BY 1
               UNTIL WS-J > 4
               ADD 1 TO WS-COUNT
           END-PERFORM.
           IF WS-COUNT = 12
               DISPLAY "NC272A-TEST-1 PASS"
           ELSE
               DISPLAY "NC272A-TEST-1 FAIL"
               DISPLAY "  Expected 12, got " WS-COUNT
           END-IF.
       NC272A-TEST-2.
      * Accumulate I*10+J for each iteration to verify order
      * I=1: J=1..4 => 11+12+13+14 = 50
      * I=2: J=1..4 => 21+22+23+24 = 90
      * I=3: J=1..4 => 31+32+33+34 = 130
      * Total = 50 + 90 + 130 = 270
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               AFTER WS-J FROM 1 BY 1
               UNTIL WS-J > 4
               COMPUTE WS-SUM = WS-SUM + WS-I * 10 + WS-J
           END-PERFORM.
           IF WS-SUM = 270
               DISPLAY "NC272A-TEST-2 PASS"
           ELSE
               DISPLAY "NC272A-TEST-2 FAIL"
               DISPLAY "  Expected 270, got " WS-SUM
           END-IF.
       NC272A-TEST-3.
      * Track last values of I and J seen inside the loop
      * Last iteration body executes: I=3, J=4
           MOVE 0 TO WS-LAST-I.
           MOVE 0 TO WS-LAST-J.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               AFTER WS-J FROM 1 BY 1
               UNTIL WS-J > 4
               MOVE WS-I TO WS-LAST-I
               MOVE WS-J TO WS-LAST-J
           END-PERFORM.
           IF WS-LAST-I = 3 AND WS-LAST-J = 4
               DISPLAY "NC272A-TEST-3 PASS"
           ELSE
               DISPLAY "NC272A-TEST-3 FAIL"
               DISPLAY "  Expected I=3 J=4, got I="
                   WS-LAST-I " J=" WS-LAST-J
           END-IF.
