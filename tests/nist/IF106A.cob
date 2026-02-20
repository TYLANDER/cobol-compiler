       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF106A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESULT           PIC 9(4).
       PROCEDURE DIVISION.
      *    TEST 1: MAX(10, 20, 30) = 30
           COMPUTE WS-RESULT =
               FUNCTION MAX(10, 20, 30)
           IF WS-RESULT = 30
               DISPLAY "IF106A-TEST-1 PASS"
           ELSE
               DISPLAY "IF106A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: MIN(10, 20, 30) = 10
           COMPUTE WS-RESULT =
               FUNCTION MIN(10, 20, 30)
           IF WS-RESULT = 10
               DISPLAY "IF106A-TEST-2 PASS"
           ELSE
               DISPLAY "IF106A-TEST-2 FAIL"
           END-IF.
      *    TEST 3: MAX of two values
           COMPUTE WS-RESULT =
               FUNCTION MAX(55, 33)
           IF WS-RESULT = 55
               DISPLAY "IF106A-TEST-3 PASS"
           ELSE
               DISPLAY "IF106A-TEST-3 FAIL"
           END-IF.
      *    TEST 4: MIN of two values
           COMPUTE WS-RESULT =
               FUNCTION MIN(55, 33)
           IF WS-RESULT = 33
               DISPLAY "IF106A-TEST-4 PASS"
           ELSE
               DISPLAY "IF106A-TEST-4 FAIL"
           END-IF.
           STOP RUN.
