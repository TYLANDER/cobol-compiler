       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC220A.
      *
      * NIST CCVS-style test: PERFORM TIMES with nested IF
      * Tests PERFORM N TIMES with conditional logic inside
      * the performed block.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER      PIC 9(4)  VALUE ZEROS.
       01 WS-EVEN-CT      PIC 9(4)  VALUE ZEROS.
       01 WS-ODD-CT       PIC 9(4)  VALUE ZEROS.
       01 WS-SUM          PIC 9(4)  VALUE ZEROS.
       01 WS-I            PIC 9(4)  VALUE ZEROS.
       01 WS-TEMP         PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: PERFORM 5 TIMES with counter and IF
      *   Increment counter 5 times, classify each as odd/even
      *   counter goes 1,2,3,4,5
      *   odd: 1,3,5 = 3, even: 2,4 = 2
           MOVE 0 TO WS-COUNTER WS-ODD-CT WS-EVEN-CT.
           PERFORM 5 TIMES
               ADD 1 TO WS-COUNTER
               MOVE WS-COUNTER TO WS-TEMP
               DIVIDE 2 INTO WS-TEMP
               MULTIPLY 2 BY WS-TEMP
               IF WS-TEMP = WS-COUNTER
                   ADD 1 TO WS-EVEN-CT
               ELSE
                   ADD 1 TO WS-ODD-CT
               END-IF
           END-PERFORM.
           IF WS-COUNTER = 5
               AND WS-ODD-CT = 3
               AND WS-EVEN-CT = 2
               DISPLAY "NC220A-TEST-1 PASS"
           ELSE
               DISPLAY "NC220A-TEST-1 FAIL"
               DISPLAY "  CTR=" WS-COUNTER
                   " ODD=" WS-ODD-CT
                   " EVEN=" WS-EVEN-CT
           END-IF.
      * Test 2: PERFORM 4 TIMES accumulating conditional sum
      *   counter goes 1,2,3,4
      *   if counter < 3 add 10, else add 20
      *   sum = 10 + 10 + 20 + 20 = 60
           MOVE 0 TO WS-COUNTER WS-SUM.
           PERFORM 4 TIMES
               ADD 1 TO WS-COUNTER
               IF WS-COUNTER < 3
                   ADD 10 TO WS-SUM
               ELSE
                   ADD 20 TO WS-SUM
               END-IF
           END-PERFORM.
           IF WS-SUM = 60 AND WS-COUNTER = 4
               DISPLAY "NC220A-TEST-2 PASS"
           ELSE
               DISPLAY "NC220A-TEST-2 FAIL"
               DISPLAY "  SUM=" WS-SUM
                   " CTR=" WS-COUNTER
           END-IF.
      * Test 3: PERFORM 3 TIMES with nested IF/ELSE chain
      *   counter goes 1,2,3
      *   if 1 add 100, if 2 add 200, else add 300
      *   sum = 100 + 200 + 300 = 600
           MOVE 0 TO WS-COUNTER WS-SUM.
           PERFORM 3 TIMES
               ADD 1 TO WS-COUNTER
               IF WS-COUNTER = 1
                   ADD 100 TO WS-SUM
               ELSE
                   IF WS-COUNTER = 2
                       ADD 200 TO WS-SUM
                   ELSE
                       ADD 300 TO WS-SUM
                   END-IF
               END-IF
           END-PERFORM.
           IF WS-SUM = 600 AND WS-COUNTER = 3
               DISPLAY "NC220A-TEST-3 PASS"
           ELSE
               DISPLAY "NC220A-TEST-3 FAIL"
               DISPLAY "  SUM=" WS-SUM
                   " CTR=" WS-COUNTER
           END-IF.
           STOP RUN.
