       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC283A.
      *
      * NIST CCVS-style test: PERFORM THRU with multiple paragraphs
      * Tests PERFORM para-a THRU para-e to verify that all
      * paragraphs in the range are executed in sequence.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER      PIC 9(4) VALUE ZEROS.
       01 WS-TRACE        PIC X(10) VALUE SPACES.
       01 WS-POS          PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC283A-CONTROL.
      * Test 1: PERFORM THRU five paragraphs
      *   Each paragraph adds 1, total should be 5
           MOVE 0 TO WS-COUNTER.
           PERFORM NC283A-PARA-A THRU NC283A-PARA-E.
           IF WS-COUNTER = 5
               DISPLAY "NC283A-TEST-1 PASS"
           ELSE
               DISPLAY "NC283A-TEST-1 FAIL"
               DISPLAY "  Expected 5, got " WS-COUNTER
           END-IF.
      * Test 2: PERFORM THRU subset of paragraphs (B thru D)
      *   Should execute B, C, D => counter incremented by 3
           MOVE 0 TO WS-COUNTER.
           PERFORM NC283A-PARA-B THRU NC283A-PARA-D.
           IF WS-COUNTER = 3
               DISPLAY "NC283A-TEST-2 PASS"
           ELSE
               DISPLAY "NC283A-TEST-2 FAIL"
               DISPLAY "  Expected 3, got " WS-COUNTER
           END-IF.
      * Test 3: PERFORM THRU verifying execution order
      *   Build trace string "ABCDE" to prove ordering
           MOVE SPACES TO WS-TRACE.
           MOVE 1 TO WS-POS.
           PERFORM NC283A-TRACE-A THRU NC283A-TRACE-E.
           IF WS-TRACE(1:5) = "ABCDE"
               DISPLAY "NC283A-TEST-3 PASS"
           ELSE
               DISPLAY "NC283A-TEST-3 FAIL"
               DISPLAY "  Expected ABCDE, got >"
                   WS-TRACE "<"
           END-IF.
           STOP RUN.
       NC283A-PARA-A.
           ADD 1 TO WS-COUNTER.
       NC283A-PARA-B.
           ADD 1 TO WS-COUNTER.
       NC283A-PARA-C.
           ADD 1 TO WS-COUNTER.
       NC283A-PARA-D.
           ADD 1 TO WS-COUNTER.
       NC283A-PARA-E.
           ADD 1 TO WS-COUNTER.
       NC283A-TRACE-A.
           MOVE "A" TO WS-TRACE(WS-POS:1).
           ADD 1 TO WS-POS.
       NC283A-TRACE-B.
           MOVE "B" TO WS-TRACE(WS-POS:1).
           ADD 1 TO WS-POS.
       NC283A-TRACE-C.
           MOVE "C" TO WS-TRACE(WS-POS:1).
           ADD 1 TO WS-POS.
       NC283A-TRACE-D.
           MOVE "D" TO WS-TRACE(WS-POS:1).
           ADD 1 TO WS-POS.
       NC283A-TRACE-E.
           MOVE "E" TO WS-TRACE(WS-POS:1).
           ADD 1 TO WS-POS.
