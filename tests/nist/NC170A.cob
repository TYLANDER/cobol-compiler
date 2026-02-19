       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC170A.
      *
      * NIST CCVS-style test: ACCEPT FROM DATE/TIME
      * Tests ACCEPT statement with FROM DATE and FROM TIME
      * to retrieve system date and time values.
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
       PROCEDURE DIVISION.
      * Test 1: ACCEPT FROM DATE returns valid date
      *   DATE format is YYMMDD
      *   Month must be 01-12, Day must be 01-31
           ACCEPT WS-DATE FROM DATE.
           IF WS-MM >= 1 AND WS-MM <= 12
               AND WS-DD >= 1 AND WS-DD <= 31
               DISPLAY "NC170A-TEST-1 PASS"
           ELSE
               DISPLAY "NC170A-TEST-1 FAIL"
               DISPLAY "  DATE=" WS-DATE
               DISPLAY "  MM=" WS-MM " DD=" WS-DD
           END-IF.
      * Test 2: ACCEPT FROM TIME returns valid time
      *   TIME format is HHMMSSHS
      *   Hour must be 00-23, Minute must be 00-59
           ACCEPT WS-TIME FROM TIME.
           IF WS-HH >= 0 AND WS-HH <= 23
               AND WS-MN >= 0 AND WS-MN <= 59
               AND WS-SS >= 0 AND WS-SS <= 59
               DISPLAY "NC170A-TEST-2 PASS"
           ELSE
               DISPLAY "NC170A-TEST-2 FAIL"
               DISPLAY "  TIME=" WS-TIME
               DISPLAY "  HH=" WS-HH " MN=" WS-MN
                   " SS=" WS-SS
           END-IF.
           STOP RUN.
