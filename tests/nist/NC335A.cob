       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC335A.
      *
      * NIST CCVS-style test: PERFORM VARYING with AFTER.
      * Tests nested varying with two loop variables using the
      * PERFORM VARYING ... AFTER syntax.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I             PIC 9(2) VALUE 0.
       01 WS-J             PIC 9(2) VALUE 0.
       01 WS-COUNT         PIC 9(4) VALUE 0.
       01 WS-SUM           PIC 9(6) VALUE 0.
       01 WS-PRODUCT       PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
       NC335A-CONTROL.
           PERFORM NC335A-TEST-1.
           PERFORM NC335A-TEST-2.
           PERFORM NC335A-TEST-3.
           STOP RUN.
       NC335A-TEST-1.
      * PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
      *     AFTER J FROM 1 BY 1 UNTIL J > 4
      *   Inner body executes 3 * 4 = 12 times
           MOVE 0 TO WS-COUNT.
           PERFORM NC335A-COUNT-PARA
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               AFTER WS-J FROM 1 BY 1 UNTIL WS-J > 4.
           IF WS-COUNT = 12
               DISPLAY "NC335A-TEST-1 PASS"
           ELSE
               DISPLAY "NC335A-TEST-1 FAIL"
               DISPLAY "  Expected 12 iterations, got " WS-COUNT
           END-IF.
       NC335A-TEST-2.
      * PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2
      *     AFTER J FROM 1 BY 1 UNTIL J > 3
      *   Sum = (1*1)+(1*2)+(1*3) + (2*1)+(2*2)+(2*3)
      *       = 1+2+3 + 2+4+6 = 6 + 12 = 18
           MOVE 0 TO WS-SUM.
           PERFORM NC335A-SUM-PARA
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 2
               AFTER WS-J FROM 1 BY 1 UNTIL WS-J > 3.
           IF WS-SUM = 18
               DISPLAY "NC335A-TEST-2 PASS"
           ELSE
               DISPLAY "NC335A-TEST-2 FAIL"
               DISPLAY "  Expected 18, got " WS-SUM
           END-IF.
       NC335A-TEST-3.
      * PERFORM VARYING I FROM 1 BY 2 UNTIL I > 5
      *     AFTER J FROM 1 BY 1 UNTIL J > 2
      *   I takes values 1,3,5. J takes 1,2 for each.
      *   Count = 3 * 2 = 6 iterations
           MOVE 0 TO WS-COUNT.
           PERFORM NC335A-COUNT-PARA
               VARYING WS-I FROM 1 BY 2 UNTIL WS-I > 5
               AFTER WS-J FROM 1 BY 1 UNTIL WS-J > 2.
           IF WS-COUNT = 6
               DISPLAY "NC335A-TEST-3 PASS"
           ELSE
               DISPLAY "NC335A-TEST-3 FAIL"
               DISPLAY "  Expected 6 iterations, got " WS-COUNT
           END-IF.
       NC335A-COUNT-PARA.
           ADD 1 TO WS-COUNT.
       NC335A-SUM-PARA.
           MULTIPLY WS-I BY WS-J GIVING WS-PRODUCT.
           ADD WS-PRODUCT TO WS-SUM.
