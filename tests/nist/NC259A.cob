       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC259A.
      *
      * NIST CCVS-style test: Nested IF with CONTINUE
      * Tests IF ... CONTINUE ... ELSE ... END-IF nesting.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC 9(4) VALUE ZEROS.
       01 WS-B        PIC 9(4) VALUE ZEROS.
       01 WS-RESULT   PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC259A-CONTROL.
           PERFORM NC259A-TEST-1.
           PERFORM NC259A-TEST-2.
           PERFORM NC259A-TEST-3.
           STOP RUN.
       NC259A-TEST-1.
      * IF true CONTINUE (no action), ELSE sets result
      * A=10 > 5 is true, so CONTINUE executes, RESULT stays 0
           MOVE 10 TO WS-A.
           MOVE 0 TO WS-RESULT.
           IF WS-A > 5
               CONTINUE
           ELSE
               MOVE 1 TO WS-RESULT
           END-IF.
           IF WS-RESULT = 0
               DISPLAY "NC259A-TEST-1 PASS"
           ELSE
               DISPLAY "NC259A-TEST-1 FAIL"
               DISPLAY "  Expected 0, got " WS-RESULT
           END-IF.
       NC259A-TEST-2.
      * IF false CONTINUE, ELSE fires
      * A=3 > 5 is false, so ELSE fires, RESULT = 2
           MOVE 3 TO WS-A.
           MOVE 0 TO WS-RESULT.
           IF WS-A > 5
               CONTINUE
           ELSE
               MOVE 2 TO WS-RESULT
           END-IF.
           IF WS-RESULT = 2
               DISPLAY "NC259A-TEST-2 PASS"
           ELSE
               DISPLAY "NC259A-TEST-2 FAIL"
               DISPLAY "  Expected 2, got " WS-RESULT
           END-IF.
       NC259A-TEST-3.
      * Nested IF with CONTINUE at multiple levels
      * A=10 B=20:
      *   outer IF A > 5 is true => CONTINUE
      *   inner IF B > 30 is false => ELSE sets RESULT=3
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 0 TO WS-RESULT.
           IF WS-A > 5
               CONTINUE
           ELSE
               MOVE 1 TO WS-RESULT
           END-IF.
           IF WS-B > 30
               CONTINUE
           ELSE
               MOVE 3 TO WS-RESULT
           END-IF.
           IF WS-RESULT = 3
               DISPLAY "NC259A-TEST-3 PASS"
           ELSE
               DISPLAY "NC259A-TEST-3 FAIL"
               DISPLAY "  Expected 3, got " WS-RESULT
           END-IF.
