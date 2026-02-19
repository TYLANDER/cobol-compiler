       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC269A.
      *
      * NIST CCVS-style test: PERFORM paragraph THRU paragraph
      * with EXIT paragraph.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER        PIC 9(4)  VALUE ZEROS.
       01 WS-RESULT          PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
       NC269A-CONTROL.
           PERFORM NC269A-TEST-1
           PERFORM NC269A-TEST-2
           PERFORM NC269A-TEST-3
           STOP RUN.
       NC269A-TEST-1.
      * PERFORM THRU with EXIT paragraph
      *   Execute STEP-A through STEP-EXIT
      *   STEP-A adds 10, STEP-B adds 20, STEP-EXIT is EXIT
      *   Result should be 30
           MOVE 0 TO WS-RESULT.
           PERFORM NC269A-STEP-A THRU NC269A-STEP-EXIT.
           IF WS-RESULT = 30
               DISPLAY "NC269A-TEST-1 PASS"
           ELSE
               DISPLAY "NC269A-TEST-1 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
           END-IF.
           GO TO NC269A-TEST-2.
       NC269A-STEP-A.
           ADD 10 TO WS-RESULT.
       NC269A-STEP-B.
           ADD 20 TO WS-RESULT.
       NC269A-STEP-EXIT.
           EXIT.
       NC269A-TEST-2.
      * PERFORM THRU with more paragraphs
      *   Execute CALC-START through CALC-EXIT
      *   CALC-START adds 100, CALC-MIDDLE adds 200
      *   CALC-EXIT is EXIT
      *   Total should be 300
           MOVE 0 TO WS-RESULT.
           PERFORM NC269A-CALC-START THRU NC269A-CALC-EXIT.
           IF WS-RESULT = 300
               DISPLAY "NC269A-TEST-2 PASS"
           ELSE
               DISPLAY "NC269A-TEST-2 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
           END-IF.
           GO TO NC269A-TEST-3.
       NC269A-CALC-START.
           ADD 100 TO WS-RESULT.
       NC269A-CALC-MIDDLE.
           ADD 200 TO WS-RESULT.
       NC269A-CALC-EXIT.
           EXIT.
       NC269A-TEST-3.
      * PERFORM THRU multiple times to accumulate
      *   Execute INCR-START through INCR-EXIT 3 times
      *   Each pass adds 5, so total should be 15
           MOVE 0 TO WS-COUNTER.
           PERFORM NC269A-INCR-START THRU NC269A-INCR-EXIT
               3 TIMES.
           IF WS-COUNTER = 15
               DISPLAY "NC269A-TEST-3 PASS"
           ELSE
               DISPLAY "NC269A-TEST-3 FAIL"
               DISPLAY "  COUNTER=" WS-COUNTER
           END-IF.
           GO TO NC269A-DONE.
       NC269A-INCR-START.
           ADD 5 TO WS-COUNTER.
       NC269A-INCR-EXIT.
           EXIT.
       NC269A-DONE.
           CONTINUE.
