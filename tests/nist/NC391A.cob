       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC391A.
      *
      * NIST CCVS-style test: ACCEPT FROM DATE and TIME
      * (Tests that accepted values are within valid ranges)
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATE            PIC 9(6) VALUE 0.
       01 WS-DATE-R.
           05 WS-YY           PIC 9(2).
           05 WS-MM           PIC 9(2).
           05 WS-DD           PIC 9(2).
       01 WS-TIME            PIC 9(8) VALUE 0.
       01 WS-TIME-R.
           05 WS-HH           PIC 9(2).
           05 WS-MN           PIC 9(2).
           05 WS-SS           PIC 9(2).
           05 WS-HS           PIC 9(2).
       01 WS-DAY             PIC 9(5) VALUE 0.
       01 WS-DAY-R.
           05 WS-DY           PIC 9(2).
           05 WS-JJJ          PIC 9(3).
       PROCEDURE DIVISION.
      * Test 1: DATE returns valid month/day
           ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO WS-DATE-R.
           IF WS-MM >= 1 AND WS-MM <= 12
               AND WS-DD >= 1 AND WS-DD <= 31
               DISPLAY "NC391A-TEST-1 PASS"
           ELSE
               DISPLAY "NC391A-TEST-1 FAIL"
               DISPLAY "  DATE=" WS-DATE
           END-IF.
      * Test 2: TIME returns valid hour/minute/second
           ACCEPT WS-TIME FROM TIME.
           MOVE WS-TIME TO WS-TIME-R.
           IF WS-HH >= 0 AND WS-HH <= 23
               AND WS-MN >= 0 AND WS-MN <= 59
               AND WS-SS >= 0 AND WS-SS <= 59
               DISPLAY "NC391A-TEST-2 PASS"
           ELSE
               DISPLAY "NC391A-TEST-2 FAIL"
               DISPLAY "  TIME=" WS-TIME
           END-IF.
      * Test 3: DAY returns valid julian day 001-366
           ACCEPT WS-DAY FROM DAY.
           MOVE WS-DAY TO WS-DAY-R.
           IF WS-JJJ >= 1 AND WS-JJJ <= 366
               DISPLAY "NC391A-TEST-3 PASS"
           ELSE
               DISPLAY "NC391A-TEST-3 FAIL"
               DISPLAY "  DAY=" WS-DAY
           END-IF.
           STOP RUN.
