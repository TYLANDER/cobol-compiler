       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF109A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESULT           PIC S9(4).
       PROCEDURE DIVISION.
      *    TEST 1: ABS(42) = 42 (positive unchanged)
           COMPUTE WS-RESULT =
               FUNCTION ABS(42)
           IF WS-RESULT = 42
               DISPLAY "IF109A-TEST-1 PASS"
           ELSE
               DISPLAY "IF109A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: ABS(0) = 0
           COMPUTE WS-RESULT =
               FUNCTION ABS(0)
           IF WS-RESULT = 0
               DISPLAY "IF109A-TEST-2 PASS"
           ELSE
               DISPLAY "IF109A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
