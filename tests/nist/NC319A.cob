       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC319A.
      *
      * NIST CCVS-style test: PERFORM UNTIL with compound condition
      * Tests PERFORM ... UNTIL with AND/OR compound conditions
      * verifying correct loop termination behavior.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(4) VALUE ZEROS.
       01 WS-B            PIC 9(4) VALUE ZEROS.
       01 WS-COUNT        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC319A-CONTROL.
           PERFORM NC319A-TEST-1.
           PERFORM NC319A-TEST-2.
           PERFORM NC319A-TEST-3.
           STOP RUN.
       NC319A-TEST-1.
      * PERFORM UNTIL A > 5 AND B < 10
      *   A starts at 0 and increments by 1 each iteration
      *   B starts at 20 and decrements by 2 each iteration
      *   Loop ends when BOTH A > 5 AND B < 10 are true
      *   Iter 1: A=1 B=18  (A>5=F => continue)
      *   Iter 2: A=2 B=16  (A>5=F => continue)
      *   Iter 3: A=3 B=14  (A>5=F => continue)
      *   Iter 4: A=4 B=12  (A>5=F => continue)
      *   Iter 5: A=5 B=10  (A>5=F => continue)
      *   Iter 6: A=6 B=8   (A>5=T B<10=T => stop)
      *   COUNT = 6
           MOVE 0 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 0 TO WS-COUNT.
           PERFORM UNTIL WS-A > 5 AND WS-B < 10
               ADD 1 TO WS-A
               SUBTRACT 2 FROM WS-B
               ADD 1 TO WS-COUNT
           END-PERFORM.
           IF WS-COUNT = 6
               DISPLAY "NC319A-TEST-1 PASS"
           ELSE
               DISPLAY "NC319A-TEST-1 FAIL"
               DISPLAY "  Expected COUNT=6"
               DISPLAY "  Got COUNT=" WS-COUNT
                   " A=" WS-A " B=" WS-B
           END-IF.
       NC319A-TEST-2.
      * Verify final values of A and B after Test 1 loop
      *   A should be 6, B should be 8
           IF WS-A = 6 AND WS-B = 8
               DISPLAY "NC319A-TEST-2 PASS"
           ELSE
               DISPLAY "NC319A-TEST-2 FAIL"
               DISPLAY "  Expected A=6 B=8"
               DISPLAY "  Got A=" WS-A " B=" WS-B
           END-IF.
       NC319A-TEST-3.
      * PERFORM UNTIL with OR condition
      *   A starts at 0, B starts at 0
      *   Loop until A > 3 OR B > 100
      *   Each iter: A+1, B+10
      *   Iter 1: A=1 B=10  (A>3=F B>100=F => continue)
      *   Iter 2: A=2 B=20  (A>3=F B>100=F => continue)
      *   Iter 3: A=3 B=30  (A>3=F B>100=F => continue)
      *   Iter 4: A=4 B=40  (A>3=T => stop)
      *   COUNT = 4
           MOVE 0 TO WS-A.
           MOVE 0 TO WS-B.
           MOVE 0 TO WS-COUNT.
           PERFORM UNTIL WS-A > 3 OR WS-B > 100
               ADD 1 TO WS-A
               ADD 10 TO WS-B
               ADD 1 TO WS-COUNT
           END-PERFORM.
           IF WS-COUNT = 4
               DISPLAY "NC319A-TEST-3 PASS"
           ELSE
               DISPLAY "NC319A-TEST-3 FAIL"
               DISPLAY "  Expected COUNT=4"
               DISPLAY "  Got COUNT=" WS-COUNT
                   " A=" WS-A " B=" WS-B
           END-IF.
