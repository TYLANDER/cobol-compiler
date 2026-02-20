       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF101A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FIELD-X10       PIC X(10) VALUE "ABCDEFGHIJ".
       01  WS-FIELD-X5        PIC X(5)  VALUE "HELLO".
       01  WS-FIELD-94        PIC 9(4)  VALUE 1234.
       01  WS-RESULT           PIC 9(4).
       PROCEDURE DIVISION.
      *    TEST 1: LENGTH of PIC X(10) field = 10
           COMPUTE WS-RESULT =
               FUNCTION LENGTH(WS-FIELD-X10)
           IF WS-RESULT = 10
               DISPLAY "IF101A-TEST-1 PASS"
           ELSE
               DISPLAY "IF101A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: LENGTH of PIC X(5) field = 5
           COMPUTE WS-RESULT =
               FUNCTION LENGTH(WS-FIELD-X5)
           IF WS-RESULT = 5
               DISPLAY "IF101A-TEST-2 PASS"
           ELSE
               DISPLAY "IF101A-TEST-2 FAIL"
           END-IF.
      *    TEST 3: LENGTH of PIC 9(4) field = 4
           COMPUTE WS-RESULT =
               FUNCTION LENGTH(WS-FIELD-94)
           IF WS-RESULT = 4
               DISPLAY "IF101A-TEST-3 PASS"
           ELSE
               DISPLAY "IF101A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
