       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC350A.
      *
      * NIST CCVS-style test: ACCEPT FROM DAY-OF-WEEK / DATE / TIME
      * Tests ACCEPT FROM DATE (YYMMDD), ACCEPT FROM TIME
      * (HHMMSSCC), and ACCEPT FROM DAY-OF-WEEK (1=Mon..7=Sun).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATE         PIC 9(6) VALUE 0.
       01 WS-DATE-X REDEFINES WS-DATE.
           05 WS-YY        PIC 9(2).
           05 WS-MM        PIC 9(2).
           05 WS-DD        PIC 9(2).
       01 WS-TIME         PIC 9(8) VALUE 0.
       01 WS-TIME-X REDEFINES WS-TIME.
           05 WS-HH        PIC 9(2).
           05 WS-MN        PIC 9(2).
           05 WS-SS        PIC 9(2).
           05 WS-HS        PIC 9(2).
       01 WS-DOW          PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: ACCEPT FROM DATE returns valid month 01-12
           ACCEPT WS-DATE FROM DATE.
           IF WS-MM >= 1 AND WS-MM <= 12
               AND WS-DD >= 1 AND WS-DD <= 31
               DISPLAY "NC350A-TEST-1 PASS"
           ELSE
               DISPLAY "NC350A-TEST-1 FAIL"
               DISPLAY "  DATE=" WS-DATE
               DISPLAY "  MM=" WS-MM " DD=" WS-DD
           END-IF.
      * Test 2: ACCEPT FROM TIME returns valid hour/minute
           ACCEPT WS-TIME FROM TIME.
           IF WS-HH >= 0 AND WS-HH <= 23
               AND WS-MN >= 0 AND WS-MN <= 59
               DISPLAY "NC350A-TEST-2 PASS"
           ELSE
               DISPLAY "NC350A-TEST-2 FAIL"
               DISPLAY "  TIME=" WS-TIME
               DISPLAY "  HH=" WS-HH " MN=" WS-MN
           END-IF.
      * Test 3: ACCEPT FROM DAY-OF-WEEK returns 1-7
           ACCEPT WS-DOW FROM DAY-OF-WEEK.
           IF WS-DOW >= 1 AND WS-DOW <= 7
               DISPLAY "NC350A-TEST-3 PASS"
           ELSE
               DISPLAY "NC350A-TEST-3 FAIL"
               DISPLAY "  DOW=" WS-DOW
           END-IF.
           STOP RUN.
