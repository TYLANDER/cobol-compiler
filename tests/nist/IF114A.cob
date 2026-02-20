       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF114A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FIELD            PIC X(8) VALUE "TestData".
       01  WS-LEN              PIC 9(4).
       01  WS-UPPER            PIC X(10).
       01  WS-MAX-VAL          PIC 9(4).
       PROCEDURE DIVISION.
      *    TEST 1: LENGTH inside COMPUTE
           COMPUTE WS-LEN =
               FUNCTION LENGTH(WS-FIELD)
           IF WS-LEN = 8
               DISPLAY "IF114A-TEST-1 PASS"
           ELSE
               DISPLAY "IF114A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: UPPER-CASE in MOVE
           MOVE FUNCTION UPPER-CASE("mixed")
               TO WS-UPPER
           IF WS-UPPER = "MIXED     "
               DISPLAY "IF114A-TEST-2 PASS"
           ELSE
               DISPLAY "IF114A-TEST-2 FAIL"
           END-IF.
      *    TEST 3: MAX with three arguments
           COMPUTE WS-MAX-VAL =
               FUNCTION MAX(5, 99, 42)
           IF WS-MAX-VAL = 99
               DISPLAY "IF114A-TEST-3 PASS"
           ELSE
               DISPLAY "IF114A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
