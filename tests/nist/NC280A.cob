       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC280A.
      *
      * NIST CCVS-style test: Multiple ADD/SUBTRACT in sequence
      * Tests multiple arithmetic operations on the same
      * variable to verify correct accumulation.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TOTAL       PIC 9(8) VALUE ZEROS.
       01 WS-A            PIC 9(4) VALUE ZEROS.
       01 WS-B            PIC 9(4) VALUE ZEROS.
       01 WS-C            PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC280A-CONTROL.
           PERFORM NC280A-TEST-1.
           PERFORM NC280A-TEST-2.
           PERFORM NC280A-TEST-3.
           STOP RUN.
       NC280A-TEST-1.
      * Sequential ADD operations on same variable
      *   0 + 10 + 20 + 30 + 40 = 100
           MOVE 0 TO WS-TOTAL.
           ADD 10 TO WS-TOTAL.
           ADD 20 TO WS-TOTAL.
           ADD 30 TO WS-TOTAL.
           ADD 40 TO WS-TOTAL.
           IF WS-TOTAL = 100
               DISPLAY "NC280A-TEST-1 PASS"
           ELSE
               DISPLAY "NC280A-TEST-1 FAIL"
               DISPLAY "  Expected 100, got " WS-TOTAL
           END-IF.
       NC280A-TEST-2.
      * Mixed ADD and SUBTRACT on same variable
      *   Start=50, +25=75, -10=65, +35=100, -50=50, +50=100
           MOVE 50 TO WS-TOTAL.
           ADD 25 TO WS-TOTAL.
           SUBTRACT 10 FROM WS-TOTAL.
           ADD 35 TO WS-TOTAL.
           SUBTRACT 50 FROM WS-TOTAL.
           ADD 50 TO WS-TOTAL.
           IF WS-TOTAL = 100
               DISPLAY "NC280A-TEST-2 PASS"
           ELSE
               DISPLAY "NC280A-TEST-2 FAIL"
               DISPLAY "  Expected 100, got " WS-TOTAL
           END-IF.
       NC280A-TEST-3.
      * ADD multiple identifiers in sequence
      *   A=100, B=200, C=300
      *   TOTAL=0, +A=100, +B=300, +C=600
      *   Then SUBTRACT A and B: 600-100-200=300
           MOVE 100 TO WS-A.
           MOVE 200 TO WS-B.
           MOVE 300 TO WS-C.
           MOVE 0 TO WS-TOTAL.
           ADD WS-A TO WS-TOTAL.
           ADD WS-B TO WS-TOTAL.
           ADD WS-C TO WS-TOTAL.
           SUBTRACT WS-A FROM WS-TOTAL.
           SUBTRACT WS-B FROM WS-TOTAL.
           IF WS-TOTAL = 300
               DISPLAY "NC280A-TEST-3 PASS"
           ELSE
               DISPLAY "NC280A-TEST-3 FAIL"
               DISPLAY "  Expected 300, got " WS-TOTAL
           END-IF.
