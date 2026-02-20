       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC371A.
      *
      * NIST CCVS-style test: PERFORM UNTIL with compound
      * conditions (AND/OR).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(3) VALUE 0.
       01 WS-B            PIC 9(3) VALUE 0.
       01 WS-COUNT        PIC 9(3) VALUE 0.
       01 WS-X            PIC 9(3) VALUE 0.
       01 WS-Y            PIC 9(3) VALUE 0.
       01 WS-COUNT2       PIC 9(3) VALUE 0.
       01 WS-M            PIC 9(3) VALUE 0.
       01 WS-N            PIC 9(3) VALUE 0.
       01 WS-COUNT3       PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: PERFORM UNTIL A > 10 AND B > 20
      * A increments by 1, B increments by 2 each iteration.
      * 11 iterations
           MOVE 0 TO WS-A.
           MOVE 0 TO WS-B.
           MOVE 0 TO WS-COUNT.
           PERFORM NC371A-PARA-1
               UNTIL WS-A > 10 AND WS-B > 20.
           IF WS-COUNT = 11
               DISPLAY "NC371A-TEST-1 PASS"
           ELSE
               DISPLAY "NC371A-TEST-1 FAIL"
               DISPLAY "  Expected 11, got " WS-COUNT
           END-IF.
      * Test 2: PERFORM UNTIL X > 5 OR Y > 8
      * X and Y increment by 1. X>5 first at iter 6.
           MOVE 0 TO WS-X.
           MOVE 0 TO WS-Y.
           MOVE 0 TO WS-COUNT2.
           PERFORM NC371A-PARA-2
               UNTIL WS-X > 5 OR WS-Y > 8.
           IF WS-COUNT2 = 6
               DISPLAY "NC371A-TEST-2 PASS"
           ELSE
               DISPLAY "NC371A-TEST-2 FAIL"
               DISPLAY "  Expected 6, got " WS-COUNT2
           END-IF.
      * Test 3: PERFORM UNTIL (M > 3 AND N > 3)
      * Both increment by 1. Both > 3 at iter 4.
           MOVE 0 TO WS-M.
           MOVE 0 TO WS-N.
           MOVE 0 TO WS-COUNT3.
           PERFORM NC371A-PARA-3
               UNTIL WS-M > 3 AND WS-N > 3.
           IF WS-COUNT3 = 4
               DISPLAY "NC371A-TEST-3 PASS"
           ELSE
               DISPLAY "NC371A-TEST-3 FAIL"
               DISPLAY "  Expected 4, got " WS-COUNT3
           END-IF.
           STOP RUN.
       NC371A-PARA-1.
           ADD 1 TO WS-A.
           ADD 2 TO WS-B.
           ADD 1 TO WS-COUNT.
       NC371A-PARA-2.
           ADD 1 TO WS-X.
           ADD 1 TO WS-Y.
           ADD 1 TO WS-COUNT2.
       NC371A-PARA-3.
           ADD 1 TO WS-M.
           ADD 1 TO WS-N.
           ADD 1 TO WS-COUNT3.
