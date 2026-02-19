       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC251A.
      *
      * NIST CCVS-style test: DIVIDE statement basic
      * Tests DIVIDE A INTO B and DIVIDE A INTO B GIVING C.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC 9(4) VALUE ZEROS.
       01 WS-B        PIC 9(4) VALUE ZEROS.
       01 WS-C        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC251A-CONTROL.
           PERFORM NC251A-TEST-1.
           PERFORM NC251A-TEST-2.
           PERFORM NC251A-TEST-3.
           STOP RUN.
       NC251A-TEST-1.
      * DIVIDE A INTO B: B becomes B / A
      *   A=5, B=100 => B = 100 / 5 = 20
           MOVE 5 TO WS-A.
           MOVE 100 TO WS-B.
           DIVIDE WS-A INTO WS-B.
           IF WS-B = 20
               DISPLAY "NC251A-TEST-1 PASS"
           ELSE
               DISPLAY "NC251A-TEST-1 FAIL"
               DISPLAY "  Expected B=20, got " WS-B
           END-IF.
       NC251A-TEST-2.
      * DIVIDE A INTO B GIVING C: C = B / A, A and B unchanged
      *   A=4, B=200 => C = 200 / 4 = 50
           MOVE 4 TO WS-A.
           MOVE 200 TO WS-B.
           MOVE 0 TO WS-C.
           DIVIDE WS-A INTO WS-B GIVING WS-C.
           IF WS-C = 50
               DISPLAY "NC251A-TEST-2 PASS"
           ELSE
               DISPLAY "NC251A-TEST-2 FAIL"
               DISPLAY "  Expected C=50, got " WS-C
           END-IF.
       NC251A-TEST-3.
      * DIVIDE literal INTO B GIVING C
      *   B=90 / 3 = 30
           MOVE 90 TO WS-B.
           MOVE 0 TO WS-C.
           DIVIDE 3 INTO WS-B GIVING WS-C.
           IF WS-C = 30
               DISPLAY "NC251A-TEST-3 PASS"
           ELSE
               DISPLAY "NC251A-TEST-3 FAIL"
               DISPLAY "  Expected C=30, got " WS-C
           END-IF.
