       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF113A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DATE-STR         PIC X(21).
       01  WS-LEN              PIC 9(4).
       01  WS-YEAR-PREFIX      PIC X(2).
       PROCEDURE DIVISION.
      *    TEST 1: CURRENT-DATE returns a 21-character string
           MOVE FUNCTION CURRENT-DATE TO WS-DATE-STR
           COMPUTE WS-LEN =
               FUNCTION LENGTH(WS-DATE-STR)
           IF WS-LEN = 21
               DISPLAY "IF113A-TEST-1 PASS"
           ELSE
               DISPLAY "IF113A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: First 2 characters are "20" (year 20xx)
           MOVE WS-DATE-STR(1:2) TO WS-YEAR-PREFIX
           IF WS-YEAR-PREFIX = "20"
               DISPLAY "IF113A-TEST-2 PASS"
           ELSE
               DISPLAY "IF113A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
