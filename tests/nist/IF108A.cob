       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF108A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESULT           PIC 9(4).
       PROCEDURE DIVISION.
      *    TEST 1: MOD(10, 3) = 1
           COMPUTE WS-RESULT =
               FUNCTION MOD(10, 3)
           IF WS-RESULT = 1
               DISPLAY "IF108A-TEST-1 PASS"
           ELSE
               DISPLAY "IF108A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: MOD(11, 4) = 3
           COMPUTE WS-RESULT =
               FUNCTION MOD(11, 4)
           IF WS-RESULT = 3
               DISPLAY "IF108A-TEST-2 PASS"
           ELSE
               DISPLAY "IF108A-TEST-2 FAIL"
           END-IF.
      *    TEST 3: MOD(42, 7) = 0
           COMPUTE WS-RESULT =
               FUNCTION MOD(42, 7)
           IF WS-RESULT = 0
               DISPLAY "IF108A-TEST-3 PASS"
           ELSE
               DISPLAY "IF108A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
