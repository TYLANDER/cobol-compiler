       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF110A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESULT           PIC S9(4).
       PROCEDURE DIVISION.
      *    TEST 1: SIGN(42) = 1 (positive)
           COMPUTE WS-RESULT =
               FUNCTION SIGN(42)
           IF WS-RESULT = 1
               DISPLAY "IF110A-TEST-1 PASS"
           ELSE
               DISPLAY "IF110A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: SIGN(0) = 0
           COMPUTE WS-RESULT =
               FUNCTION SIGN(0)
           IF WS-RESULT = 0
               DISPLAY "IF110A-TEST-2 PASS"
           ELSE
               DISPLAY "IF110A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
