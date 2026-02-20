       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF111A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESULT           PIC 9(4).
       PROCEDURE DIVISION.
      *    TEST 1: INTEGER(42) = 42
           COMPUTE WS-RESULT =
               FUNCTION INTEGER(42)
           IF WS-RESULT = 42
               DISPLAY "IF111A-TEST-1 PASS"
           ELSE
               DISPLAY "IF111A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: INTEGER-PART(99) = 99
           COMPUTE WS-RESULT =
               FUNCTION INTEGER-PART(99)
           IF WS-RESULT = 99
               DISPLAY "IF111A-TEST-2 PASS"
           ELSE
               DISPLAY "IF111A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
