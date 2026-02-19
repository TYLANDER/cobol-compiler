       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC252A.
      *
      * NIST CCVS-style test: DIVIDE with REMAINDER
      * Tests DIVIDE A BY B GIVING C REMAINDER D.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC 9(4) VALUE ZEROS.
       01 WS-B        PIC 9(4) VALUE ZEROS.
       01 WS-Q        PIC 9(4) VALUE ZEROS.
       01 WS-R        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC252A-CONTROL.
           PERFORM NC252A-TEST-1.
           PERFORM NC252A-TEST-2.
           PERFORM NC252A-TEST-3.
           STOP RUN.
       NC252A-TEST-1.
      * DIVIDE 17 BY 5 GIVING Q REMAINDER R
      *   Q = 3, R = 2
           MOVE 17 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE 0 TO WS-Q.
           MOVE 0 TO WS-R.
           DIVIDE WS-A BY WS-B GIVING WS-Q
               REMAINDER WS-R.
           IF WS-Q = 3 AND WS-R = 2
               DISPLAY "NC252A-TEST-1 PASS"
           ELSE
               DISPLAY "NC252A-TEST-1 FAIL"
               DISPLAY "  Expected Q=3 R=2, got Q="
                   WS-Q " R=" WS-R
           END-IF.
       NC252A-TEST-2.
      * DIVIDE 100 BY 7 GIVING Q REMAINDER R
      *   Q = 14, R = 2
           MOVE 100 TO WS-A.
           MOVE 7 TO WS-B.
           MOVE 0 TO WS-Q.
           MOVE 0 TO WS-R.
           DIVIDE WS-A BY WS-B GIVING WS-Q
               REMAINDER WS-R.
           IF WS-Q = 14 AND WS-R = 2
               DISPLAY "NC252A-TEST-2 PASS"
           ELSE
               DISPLAY "NC252A-TEST-2 FAIL"
               DISPLAY "  Expected Q=14 R=2, got Q="
                   WS-Q " R=" WS-R
           END-IF.
       NC252A-TEST-3.
      * DIVIDE 20 BY 4 GIVING Q REMAINDER R (exact division)
      *   Q = 5, R = 0
           MOVE 20 TO WS-A.
           MOVE 4 TO WS-B.
           MOVE 0 TO WS-Q.
           MOVE 0 TO WS-R.
           DIVIDE WS-A BY WS-B GIVING WS-Q
               REMAINDER WS-R.
           IF WS-Q = 5 AND WS-R = 0
               DISPLAY "NC252A-TEST-3 PASS"
           ELSE
               DISPLAY "NC252A-TEST-3 FAIL"
               DISPLAY "  Expected Q=5 R=0, got Q="
                   WS-Q " R=" WS-R
           END-IF.
