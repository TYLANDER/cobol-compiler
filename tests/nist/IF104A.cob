       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF104A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FIELD            PIC X(5) VALUE "ABCDE".
       01  WS-RESULT           PIC X(5).
       PROCEDURE DIVISION.
      *    TEST 1: REVERSE of literal "ABCDE" = "EDCBA"
           MOVE FUNCTION REVERSE("ABCDE")
               TO WS-RESULT
           IF WS-RESULT = "EDCBA"
               DISPLAY "IF104A-TEST-1 PASS"
           ELSE
               DISPLAY "IF104A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: REVERSE of a data field
           MOVE FUNCTION REVERSE(WS-FIELD)
               TO WS-RESULT
           IF WS-RESULT = "EDCBA"
               DISPLAY "IF104A-TEST-2 PASS"
           ELSE
               DISPLAY "IF104A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
