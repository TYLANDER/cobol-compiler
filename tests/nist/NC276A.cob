       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC276A.
      *
      * NIST CCVS-style test: Level-88 condition names with
      * VALUE THRU ranges. Tests condition name evaluation
      * for single values, multiple values, and THRU ranges.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SCORE       PIC 9(3) VALUE ZEROS.
           88 WS-FAILING   VALUE 0 THRU 59.
           88 WS-PASSING   VALUE 60 THRU 100.
       01 WS-GRADE-LTR   PIC X VALUE SPACES.
           88 WS-GRADE-A   VALUE "A".
           88 WS-GRADE-B   VALUE "B".
           88 WS-GRADE-F   VALUE "F".
       01 WS-MONTH        PIC 9(2) VALUE ZEROS.
           88 WS-QUARTER1  VALUE 1 THRU 3.
           88 WS-QUARTER2  VALUE 4 THRU 6.
           88 WS-QUARTER3  VALUE 7 THRU 9.
           88 WS-QUARTER4  VALUE 10 THRU 12.
       PROCEDURE DIVISION.
       NC276A-CONTROL.
           PERFORM NC276A-TEST-1.
           PERFORM NC276A-TEST-2.
           PERFORM NC276A-TEST-3.
           STOP RUN.
       NC276A-TEST-1.
      * Test VALUE THRU range on numeric
      *   SCORE=45 should satisfy WS-FAILING (0 THRU 59)
      *   SCORE=75 should satisfy WS-PASSING (60 THRU 100)
           MOVE 45 TO WS-SCORE.
           IF WS-FAILING
               MOVE 75 TO WS-SCORE
               IF WS-PASSING
                   DISPLAY "NC276A-TEST-1 PASS"
               ELSE
                   DISPLAY "NC276A-TEST-1 FAIL"
                   DISPLAY "  75 not PASSING"
               END-IF
           ELSE
               DISPLAY "NC276A-TEST-1 FAIL"
               DISPLAY "  45 not FAILING"
           END-IF.
       NC276A-TEST-2.
      * Test single-value level-88 on alphanumeric
      *   SET WS-GRADE-A => GRADE-LTR = "A"
      *   Then SET WS-GRADE-F => GRADE-LTR = "F"
           SET WS-GRADE-A TO TRUE.
           IF WS-GRADE-A
               SET WS-GRADE-F TO TRUE
               IF WS-GRADE-F AND WS-GRADE-LTR = "F"
                   DISPLAY "NC276A-TEST-2 PASS"
               ELSE
                   DISPLAY "NC276A-TEST-2 FAIL"
                   DISPLAY "  GRADE-LTR=>" WS-GRADE-LTR "<"
               END-IF
           ELSE
               DISPLAY "NC276A-TEST-2 FAIL"
               DISPLAY "  SET GRADE-A failed"
           END-IF.
       NC276A-TEST-3.
      * Test THRU ranges for quarters of the year
      *   Month=2 => Q1, Month=5 => Q2, Month=11 => Q4
           MOVE 2 TO WS-MONTH.
           IF WS-QUARTER1
               MOVE 5 TO WS-MONTH
               IF WS-QUARTER2
                   MOVE 11 TO WS-MONTH
                   IF WS-QUARTER4
                       DISPLAY "NC276A-TEST-3 PASS"
                   ELSE
                       DISPLAY "NC276A-TEST-3 FAIL"
                       DISPLAY "  Month=11 not Q4"
                   END-IF
               ELSE
                   DISPLAY "NC276A-TEST-3 FAIL"
                   DISPLAY "  Month=5 not Q2"
               END-IF
           ELSE
               DISPLAY "NC276A-TEST-3 FAIL"
               DISPLAY "  Month=2 not Q1"
           END-IF.
