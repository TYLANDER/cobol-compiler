       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC374A.
      *
      * NIST CCVS-style test: ACCEPT FROM DAY-OF-WEEK
      * returns a value between 1 and 7.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DAY          PIC 9     VALUE 0.
       01 WS-DAY-NUM      PIC 9(3)  VALUE 0.
       01 WS-DAY2         PIC 9     VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: ACCEPT DAY-OF-WEEK returns value 1 through 7
           ACCEPT WS-DAY FROM DAY-OF-WEEK.
           IF WS-DAY >= 1 AND WS-DAY <= 7
               DISPLAY "NC374A-TEST-1 PASS"
           ELSE
               DISPLAY "NC374A-TEST-1 FAIL"
               DISPLAY "  Expected 1-7, got " WS-DAY
           END-IF.
      * Test 2: ACCEPT DAY-OF-WEEK is numeric
           MOVE WS-DAY TO WS-DAY-NUM.
           IF WS-DAY-NUM >= 1 AND WS-DAY-NUM <= 7
               DISPLAY "NC374A-TEST-2 PASS"
           ELSE
               DISPLAY "NC374A-TEST-2 FAIL"
               DISPLAY "  Expected 1-7, got " WS-DAY-NUM
           END-IF.
      * Test 3: Two successive ACCEPTs return the same value
           ACCEPT WS-DAY2 FROM DAY-OF-WEEK.
           IF WS-DAY2 = WS-DAY
               DISPLAY "NC374A-TEST-3 PASS"
           ELSE
               DISPLAY "NC374A-TEST-3 FAIL"
               DISPLAY "  First=" WS-DAY " Second=" WS-DAY2
           END-IF.
           STOP RUN.
