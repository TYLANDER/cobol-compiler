       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF107A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESULT           PIC 9(4).
       PROCEDURE DIVISION.
      *    TEST 1: ORD("A") = 66 (ASCII 65 + 1)
           COMPUTE WS-RESULT =
               FUNCTION ORD("A")
           IF WS-RESULT = 66
               DISPLAY "IF107A-TEST-1 PASS"
           ELSE
               DISPLAY "IF107A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: ORD("Z") = 91 (ASCII 90 + 1)
           COMPUTE WS-RESULT =
               FUNCTION ORD("Z")
           IF WS-RESULT = 91
               DISPLAY "IF107A-TEST-2 PASS"
           ELSE
               DISPLAY "IF107A-TEST-2 FAIL"
           END-IF.
      *    TEST 3: ORD(" ") = 33 (ASCII 32 + 1)
           COMPUTE WS-RESULT =
               FUNCTION ORD(" ")
           IF WS-RESULT = 33
               DISPLAY "IF107A-TEST-3 PASS"
           ELSE
               DISPLAY "IF107A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
