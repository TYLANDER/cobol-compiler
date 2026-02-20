       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC387A.
      *
      * NIST CCVS-style test: PERFORM TIMES
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNT           PIC 9(3) VALUE 0.
       01 WS-TIMES           PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: PERFORM 5 TIMES
           MOVE 0 TO WS-COUNT.
           PERFORM NC387A-PARA-1 5 TIMES.
           IF WS-COUNT = 5
               DISPLAY "NC387A-TEST-1 PASS"
           ELSE
               DISPLAY "NC387A-TEST-1 FAIL"
               DISPLAY "  Expected 5, got " WS-COUNT
           END-IF.
      * Test 2: PERFORM variable TIMES (10)
           MOVE 0 TO WS-COUNT.
           MOVE 10 TO WS-TIMES.
           PERFORM NC387A-PARA-1 WS-TIMES TIMES.
           IF WS-COUNT = 10
               DISPLAY "NC387A-TEST-2 PASS"
           ELSE
               DISPLAY "NC387A-TEST-2 FAIL"
               DISPLAY "  Expected 10, got " WS-COUNT
           END-IF.
      * Test 3: PERFORM 0 TIMES (should not execute)
           MOVE 0 TO WS-COUNT.
           PERFORM NC387A-PARA-1 0 TIMES.
           IF WS-COUNT = 0
               DISPLAY "NC387A-TEST-3 PASS"
           ELSE
               DISPLAY "NC387A-TEST-3 FAIL"
               DISPLAY "  Expected 0, got " WS-COUNT
           END-IF.
           STOP RUN.
       NC387A-PARA-1.
           ADD 1 TO WS-COUNT.
