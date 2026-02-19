       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC310A.
      *
      * NIST CCVS-style test: PERFORM paragraph 0 TIMES
      * Tests that a paragraph performed 0 times does not execute
      * its body, verifying counters remain unchanged.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER    PIC 9(4) VALUE ZEROS.
       01 WS-FLAG       PIC 9    VALUE 0.
       01 WS-ACCUM      PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC310A-CONTROL.
           PERFORM NC310A-TEST-1.
           PERFORM NC310A-TEST-2.
           PERFORM NC310A-TEST-3.
           STOP RUN.
       NC310A-TEST-1.
      * PERFORM a paragraph 0 TIMES
      *   Counter should remain 0
           MOVE 0 TO WS-COUNTER.
           PERFORM NC310A-ADD-PARA 0 TIMES.
           IF WS-COUNTER = 0
               DISPLAY "NC310A-TEST-1 PASS"
           ELSE
               DISPLAY "NC310A-TEST-1 FAIL"
               DISPLAY "  Expected 0, got " WS-COUNTER
           END-IF.
       NC310A-TEST-2.
      * PERFORM a paragraph 1 TIMES then 0 TIMES
      *   Counter goes to 10 after 1 TIMES, stays at 10 after 0
           MOVE 0 TO WS-COUNTER.
           PERFORM NC310A-ADD-PARA 1 TIMES.
           PERFORM NC310A-ADD-PARA 0 TIMES.
           IF WS-COUNTER = 10
               DISPLAY "NC310A-TEST-2 PASS"
           ELSE
               DISPLAY "NC310A-TEST-2 FAIL"
               DISPLAY "  Expected 10, got " WS-COUNTER
           END-IF.
       NC310A-TEST-3.
      * PERFORM THRU 0 TIMES verifying two paragraphs skipped
      *   Flag and accum should remain at initial values
           MOVE 0 TO WS-FLAG.
           MOVE 0 TO WS-ACCUM.
           PERFORM NC310A-FLAG-PARA THRU NC310A-ACCUM-PARA
               0 TIMES.
           IF WS-FLAG = 0 AND WS-ACCUM = 0
               DISPLAY "NC310A-TEST-3 PASS"
           ELSE
               DISPLAY "NC310A-TEST-3 FAIL"
               DISPLAY "  Expected FLAG=0 ACCUM=0"
               DISPLAY "  Got FLAG=" WS-FLAG
                   " ACCUM=" WS-ACCUM
           END-IF.
       NC310A-ADD-PARA.
           ADD 10 TO WS-COUNTER.
       NC310A-FLAG-PARA.
           MOVE 1 TO WS-FLAG.
       NC310A-ACCUM-PARA.
           ADD 50 TO WS-ACCUM.
