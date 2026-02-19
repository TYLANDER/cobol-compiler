       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC180A.
      *
      * NIST CCVS-style test: PERFORM with AFTER clause
      * Tests nested looping via PERFORM VARYING ... AFTER
      * to iterate over multiple indices simultaneously.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I             PIC 9(2) VALUE ZEROS.
       01 WS-J             PIC 9(2) VALUE ZEROS.
       01 WS-COUNT         PIC 9(4) VALUE ZEROS.
       01 WS-SUM           PIC 9(6) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: PERFORM VARYING I AFTER J counts correctly
      *   I from 1 by 1 until I > 3
      *   AFTER J from 1 by 1 until J > 4
      *   Total iterations = 3 * 4 = 12
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               AFTER WS-J FROM 1 BY 1
               UNTIL WS-J > 4
               ADD 1 TO WS-COUNT
           END-PERFORM.
           IF WS-COUNT = 12
               DISPLAY "NC180A-TEST-1 PASS"
           ELSE
               DISPLAY "NC180A-TEST-1 FAIL"
               DISPLAY "  Expected 12 got " WS-COUNT
           END-IF.
      * Test 2: Verify sum of products I*J
      *   I=1..3, J=1..4
      *   Sum = (1*1+1*2+1*3+1*4)+(2*1+2*2+2*3+2*4)
      *        +(3*1+3*2+3*3+3*4)
      *       = 10 + 20 + 30 = 60
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               AFTER WS-J FROM 1 BY 1
               UNTIL WS-J > 4
               COMPUTE WS-SUM = WS-SUM +
                   (WS-I * WS-J)
           END-PERFORM.
           IF WS-SUM = 60
               DISPLAY "NC180A-TEST-2 PASS"
           ELSE
               DISPLAY "NC180A-TEST-2 FAIL"
               DISPLAY "  Expected 60 got " WS-SUM
           END-IF.
           STOP RUN.
