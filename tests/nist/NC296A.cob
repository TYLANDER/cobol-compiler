       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC296A.
      *
      * NIST CCVS-style test: ACCEPT FROM DATE/TIME/DAY
      * Tests ACCEPT statement with FROM DATE, FROM TIME,
      * and FROM DAY to retrieve system date and time.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATE         PIC 9(6) VALUE ZEROS.
       01 WS-DATE-X REDEFINES WS-DATE.
           05 WS-YY        PIC 9(2).
           05 WS-MM        PIC 9(2).
           05 WS-DD        PIC 9(2).
       01 WS-TIME         PIC 9(8) VALUE ZEROS.
       01 WS-TIME-X REDEFINES WS-TIME.
           05 WS-HH        PIC 9(2).
           05 WS-MN        PIC 9(2).
           05 WS-SS        PIC 9(2).
           05 WS-HS        PIC 9(2).
       01 WS-DAY          PIC 9(5) VALUE ZEROS.
       01 WS-DAY-X REDEFINES WS-DAY.
           05 WS-DY-YY     PIC 9(2).
           05 WS-DY-DDD    PIC 9(3).
       PROCEDURE DIVISION.
      * Test 1: ACCEPT FROM DATE returns valid month
      *   DATE format is YYMMDD, month must be 01-12
           ACCEPT WS-DATE FROM DATE.
           IF WS-MM >= 1 AND WS-MM <= 12
               DISPLAY "NC296A-TEST-1 PASS"
           ELSE
               DISPLAY "NC296A-TEST-1 FAIL"
               DISPLAY "  DATE=" WS-DATE
               DISPLAY "  MM=" WS-MM
           END-IF.
      * Test 2: ACCEPT FROM TIME returns valid hour/minute
      *   TIME format is HHMMSSHS
      *   Hour 00-23, Minute 00-59
           ACCEPT WS-TIME FROM TIME.
           IF WS-HH >= 0 AND WS-HH <= 23
               AND WS-MN >= 0 AND WS-MN <= 59
               DISPLAY "NC296A-TEST-2 PASS"
           ELSE
               DISPLAY "NC296A-TEST-2 FAIL"
               DISPLAY "  TIME=" WS-TIME
               DISPLAY "  HH=" WS-HH " MN=" WS-MN
           END-IF.
      * Test 3: ACCEPT FROM DAY returns valid day of year
      *   DAY format is YYDDD, day must be 001-366
           ACCEPT WS-DAY FROM DAY.
           IF WS-DY-DDD >= 1 AND WS-DY-DDD <= 366
               DISPLAY "NC296A-TEST-3 PASS"
           ELSE
               DISPLAY "NC296A-TEST-3 FAIL"
               DISPLAY "  DAY=" WS-DAY
               DISPLAY "  DDD=" WS-DY-DDD
           END-IF.
           STOP RUN.
