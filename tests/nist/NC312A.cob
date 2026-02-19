       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC312A.
      *
      * NIST CCVS-style test: PERFORM paragraph THRU with TIMES
      * Tests PERFORM para-A THRU para-B N TIMES to verify
      * both paragraphs execute the correct number of times.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER      PIC 9(4) VALUE ZEROS.
       01 WS-FLAG          PIC 9(4) VALUE ZEROS.
       01 WS-ACCUM         PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC312A-CONTROL.
           PERFORM NC312A-TEST-1.
           PERFORM NC312A-TEST-2.
           PERFORM NC312A-TEST-3.
           STOP RUN.
       NC312A-TEST-1.
      * PERFORM PARA-A THRU PARA-B 3 TIMES
      *   PARA-A adds 10, PARA-B adds 5
      *   3 iterations => COUNTER = 30, FLAG = 15
           MOVE 0 TO WS-COUNTER.
           MOVE 0 TO WS-FLAG.
           PERFORM NC312A-PARA-A THRU NC312A-PARA-B
               3 TIMES.
           IF WS-COUNTER = 30 AND WS-FLAG = 15
               DISPLAY "NC312A-TEST-1 PASS"
           ELSE
               DISPLAY "NC312A-TEST-1 FAIL"
               DISPLAY "  Expected CTR=30 FLAG=15"
               DISPLAY "  Got CTR=" WS-COUNTER
                   " FLAG=" WS-FLAG
           END-IF.
       NC312A-TEST-2.
      * PERFORM PARA-A THRU PARA-B 1 TIMES
      *   1 iteration => COUNTER = 10, FLAG = 5
           MOVE 0 TO WS-COUNTER.
           MOVE 0 TO WS-FLAG.
           PERFORM NC312A-PARA-A THRU NC312A-PARA-B
               1 TIMES.
           IF WS-COUNTER = 10 AND WS-FLAG = 5
               DISPLAY "NC312A-TEST-2 PASS"
           ELSE
               DISPLAY "NC312A-TEST-2 FAIL"
               DISPLAY "  Expected CTR=10 FLAG=5"
               DISPLAY "  Got CTR=" WS-COUNTER
                   " FLAG=" WS-FLAG
           END-IF.
       NC312A-TEST-3.
      * PERFORM PARA-A THRU PARA-B 5 TIMES
      *   5 iterations => COUNTER = 50, FLAG = 25
           MOVE 0 TO WS-COUNTER.
           MOVE 0 TO WS-FLAG.
           PERFORM NC312A-PARA-A THRU NC312A-PARA-B
               5 TIMES.
           IF WS-COUNTER = 50 AND WS-FLAG = 25
               DISPLAY "NC312A-TEST-3 PASS"
           ELSE
               DISPLAY "NC312A-TEST-3 FAIL"
               DISPLAY "  Expected CTR=50 FLAG=25"
               DISPLAY "  Got CTR=" WS-COUNTER
                   " FLAG=" WS-FLAG
           END-IF.
       NC312A-PARA-A.
           ADD 10 TO WS-COUNTER.
       NC312A-PARA-B.
           ADD 5 TO WS-FLAG.
