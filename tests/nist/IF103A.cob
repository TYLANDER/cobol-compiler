       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF103A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MIXED            PIC X(10) VALUE "HeLLo WoRd".
       01  WS-RESULT           PIC X(10).
       01  WS-RESULT5          PIC X(5).
       PROCEDURE DIVISION.
      *    TEST 1: LOWER-CASE of literal "HELLO" = "hello"
           MOVE FUNCTION LOWER-CASE("HELLO")
               TO WS-RESULT5
           IF WS-RESULT5 = "hello"
               DISPLAY "IF103A-TEST-1 PASS"
           ELSE
               DISPLAY "IF103A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: LOWER-CASE of mixed-case field
           MOVE FUNCTION LOWER-CASE(WS-MIXED)
               TO WS-RESULT
           IF WS-RESULT = "hello word"
               DISPLAY "IF103A-TEST-2 PASS"
           ELSE
               DISPLAY "IF103A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
