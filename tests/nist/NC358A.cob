       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC358A.
      *
      * NIST CCVS-style test: Nested PERFORM with multiple
      * counters. PERFORM VARYING with inner loop,
      * counting total iterations.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I              PIC 9(2) VALUE 0.
       01 WS-J              PIC 9(2) VALUE 0.
       01 WS-COUNT          PIC 9(4) VALUE 0.
       01 WS-OUTER-COUNT    PIC 9(4) VALUE 0.
       01 WS-PRODUCT-SUM    PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Nested PERFORM VARYING counts iterations
      *   Outer: I from 1 to 3, Inner: J from 1 to 4.
      *   Total iterations should be 3 * 4 = 12.
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > 4
                   ADD 1 TO WS-COUNT
               END-PERFORM
           END-PERFORM.
           IF WS-COUNT = 12
               DISPLAY "NC358A-TEST-1 PASS"
           ELSE
               DISPLAY "NC358A-TEST-1 FAIL"
               DISPLAY "  Expected 12, got " WS-COUNT
           END-IF.
      * Test 2: Verify outer loop counter after completion
      *   After PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3,
      *   I should be 4 (the value that caused exit).
           MOVE 0 TO WS-OUTER-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               ADD 1 TO WS-OUTER-COUNT
           END-PERFORM.
           IF WS-I = 4 AND WS-OUTER-COUNT = 3
               DISPLAY "NC358A-TEST-2 PASS"
           ELSE
               DISPLAY "NC358A-TEST-2 FAIL"
               DISPLAY "  Expected I=4 count=3, got I="
                       WS-I " count=" WS-OUTER-COUNT
           END-IF.
      * Test 3: Nested loop accumulating products
      *   Sum of I*J for I=1..3, J=1..3.
      *   = (1*1+1*2+1*3)+(2*1+2*2+2*3)+(3*1+3*2+3*3)
      *   = 6 + 12 + 18 = 36.
           MOVE 0 TO WS-PRODUCT-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > 3
                   COMPUTE WS-PRODUCT-SUM =
                       WS-PRODUCT-SUM + (WS-I * WS-J)
               END-PERFORM
           END-PERFORM.
           IF WS-PRODUCT-SUM = 36
               DISPLAY "NC358A-TEST-3 PASS"
           ELSE
               DISPLAY "NC358A-TEST-3 FAIL"
               DISPLAY "  Expected 36, got "
                       WS-PRODUCT-SUM
           END-IF.
           STOP RUN.
