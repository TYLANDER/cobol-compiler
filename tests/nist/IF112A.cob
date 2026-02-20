       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF112A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESULT           PIC 9(4).
       PROCEDURE DIVISION.
      *    TEST 1: NUMVAL("  123  ") converts to 123
           COMPUTE WS-RESULT =
               FUNCTION NUMVAL("  123  ")
           IF WS-RESULT = 123
               DISPLAY "IF112A-TEST-1 PASS"
           ELSE
               DISPLAY "IF112A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: NUMVAL("456") converts to 456
           COMPUTE WS-RESULT =
               FUNCTION NUMVAL("456")
           IF WS-RESULT = 456
               DISPLAY "IF112A-TEST-2 PASS"
           ELSE
               DISPLAY "IF112A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
