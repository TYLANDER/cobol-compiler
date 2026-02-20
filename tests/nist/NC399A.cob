       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC399A.
      *
      * NIST CCVS-style test: CONTINUE statement and paragraph flow
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNT1          PIC 9(3) VALUE 0.
       01 WS-COUNT2          PIC 9(3) VALUE 0.
       01 WS-COUNT3          PIC 9(3) VALUE 0.
       01 WS-I               PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: CONTINUE in IF does nothing
           MOVE 0 TO WS-COUNT1.
           IF WS-COUNT1 = 0
               CONTINUE
           END-IF.
           ADD 1 TO WS-COUNT1.
           IF WS-COUNT1 = 1
               DISPLAY "NC399A-TEST-1 PASS"
           ELSE
               DISPLAY "NC399A-TEST-1 FAIL"
               DISPLAY "  COUNT1=" WS-COUNT1
           END-IF.
      * Test 2: CONTINUE in ELSE branch
           MOVE 5 TO WS-COUNT2.
           IF WS-COUNT2 = 0
               ADD 10 TO WS-COUNT2
           ELSE
               CONTINUE
           END-IF.
           IF WS-COUNT2 = 5
               DISPLAY "NC399A-TEST-2 PASS"
           ELSE
               DISPLAY "NC399A-TEST-2 FAIL"
               DISPLAY "  COUNT2=" WS-COUNT2
           END-IF.
      * Test 3: PERFORM paragraph and paragraph falls through
           MOVE 0 TO WS-COUNT3.
           PERFORM NC399A-PARA-A.
           PERFORM NC399A-PARA-B.
           IF WS-COUNT3 = 3
               DISPLAY "NC399A-TEST-3 PASS"
           ELSE
               DISPLAY "NC399A-TEST-3 FAIL"
               DISPLAY "  COUNT3=" WS-COUNT3
           END-IF.
           STOP RUN.
       NC399A-PARA-A.
           ADD 1 TO WS-COUNT3.
       NC399A-PARA-B.
           ADD 2 TO WS-COUNT3.
