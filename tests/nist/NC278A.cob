       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC278A.
      *
      * NIST CCVS-style test: PERFORM VARYING with different
      * FROM/BY/UNTIL combinations. Tests inline PERFORM
      * VARYING with various step sizes and conditions.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-IDX         PIC 9(4) VALUE ZEROS.
       01 WS-SUM         PIC 9(8) VALUE ZEROS.
       01 WS-COUNT       PIC 9(4) VALUE ZEROS.
       01 WS-LAST-VAL    PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC278A-CONTROL.
           PERFORM NC278A-TEST-1.
           PERFORM NC278A-TEST-2.
           PERFORM NC278A-TEST-3.
           STOP RUN.
       NC278A-TEST-1.
      * PERFORM VARYING FROM 1 BY 1 UNTIL > 5
      *   Sum = 1+2+3+4+5 = 15
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > 5
               ADD WS-IDX TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 15
               DISPLAY "NC278A-TEST-1 PASS"
           ELSE
               DISPLAY "NC278A-TEST-1 FAIL"
               DISPLAY "  Expected 15, got " WS-SUM
           END-IF.
       NC278A-TEST-2.
      * PERFORM VARYING FROM 2 BY 3 UNTIL > 20
      *   Values: 2, 5, 8, 11, 14, 17, 20 (7 iterations)
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-IDX FROM 2 BY 3
               UNTIL WS-IDX > 20
               ADD 1 TO WS-COUNT
           END-PERFORM.
           IF WS-COUNT = 7
               DISPLAY "NC278A-TEST-2 PASS"
           ELSE
               DISPLAY "NC278A-TEST-2 FAIL"
               DISPLAY "  Expected 7, got " WS-COUNT
           END-IF.
       NC278A-TEST-3.
      * PERFORM VARYING FROM 10 BY 10 UNTIL > 50
      *   Values: 10, 20, 30, 40, 50 (5 iterations)
      *   Sum = 10+20+30+40+50 = 150
           MOVE 0 TO WS-SUM.
           MOVE 0 TO WS-LAST-VAL.
           PERFORM VARYING WS-IDX FROM 10 BY 10
               UNTIL WS-IDX > 50
               ADD WS-IDX TO WS-SUM
               MOVE WS-IDX TO WS-LAST-VAL
           END-PERFORM.
           IF WS-SUM = 150 AND WS-LAST-VAL = 50
               DISPLAY "NC278A-TEST-3 PASS"
           ELSE
               DISPLAY "NC278A-TEST-3 FAIL"
               DISPLAY "  Expected SUM=150 LAST=50, got SUM="
                   WS-SUM " LAST=" WS-LAST-VAL
           END-IF.
