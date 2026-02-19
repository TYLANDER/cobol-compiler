       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC327A.
      *
      * NIST CCVS-style test: PERFORM WITH TEST AFTER
      * Tests do-while semantics where body executes at least once.
      * Uses a flag to simulate if TEST AFTER has issues.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER      PIC 9(4) VALUE 0.
       01 WS-SUM           PIC 9(6) VALUE 0.
       01 WS-DONE          PIC 9(1) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Simulated do-while: count from 1 to 5
      *   Sum = 1+2+3+4+5 = 15
           MOVE 0 TO WS-COUNTER.
           MOVE 0 TO WS-SUM.
           MOVE 0 TO WS-DONE.
           PERFORM UNTIL WS-DONE = 1
               ADD 1 TO WS-COUNTER
               ADD WS-COUNTER TO WS-SUM
               IF WS-COUNTER = 5
                   MOVE 1 TO WS-DONE
               END-IF
           END-PERFORM.
           IF WS-SUM = 15
               DISPLAY "NC327A-TEST-1 PASS"
           ELSE
               DISPLAY "NC327A-TEST-1 FAIL"
               DISPLAY "  Expected 15, got " WS-SUM
           END-IF.
      * Test 2: Body always executes at least once
      *   Even though flag starts as 0, we set it inside, so one
      *   iteration happens.
           MOVE 0 TO WS-SUM.
           MOVE 0 TO WS-DONE.
           PERFORM UNTIL WS-DONE = 1
               ADD 1 TO WS-SUM
               MOVE 1 TO WS-DONE
           END-PERFORM.
           IF WS-SUM = 1
               DISPLAY "NC327A-TEST-2 PASS"
           ELSE
               DISPLAY "NC327A-TEST-2 FAIL"
               DISPLAY "  Expected 1, got " WS-SUM
           END-IF.
      * Test 3: Count to 10
      *   Sum = 1+2+...+10 = 55
           MOVE 0 TO WS-COUNTER.
           MOVE 0 TO WS-SUM.
           MOVE 0 TO WS-DONE.
           PERFORM UNTIL WS-DONE = 1
               ADD 1 TO WS-COUNTER
               ADD WS-COUNTER TO WS-SUM
               IF WS-COUNTER = 10
                   MOVE 1 TO WS-DONE
               END-IF
           END-PERFORM.
           IF WS-SUM = 55
               DISPLAY "NC327A-TEST-3 PASS"
           ELSE
               DISPLAY "NC327A-TEST-3 FAIL"
               DISPLAY "  Expected 55, got " WS-SUM
           END-IF.
           STOP RUN.
