       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC234A.
      *
      * NIST CCVS-style test: PERFORM VARYING with AFTER clause
      * Tests nested varying using PERFORM VARYING ... AFTER ...
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I        PIC 9(4) VALUE ZEROS.
       01 WS-J        PIC 9(4) VALUE ZEROS.
       01 WS-TOTAL    PIC 9(8) VALUE ZEROS.
       01 WS-PRODUCT  PIC 9(8) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
      *         AFTER J FROM 1 BY 1 UNTIL J > 4
      *   Outer loops 3 times, inner loops 4 times each
      *   Total iterations = 3 * 4 = 12
           MOVE 0 TO WS-TOTAL.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               AFTER WS-J FROM 1 BY 1
               UNTIL WS-J > 4
               ADD 1 TO WS-TOTAL
           END-PERFORM.
           IF WS-TOTAL = 12
               DISPLAY "NC234A-TEST-1 PASS"
           ELSE
               DISPLAY "NC234A-TEST-1 FAIL"
               DISPLAY "  Expected 12, got " WS-TOTAL
           END-IF.
      * Test 2: PERFORM VARYING with AFTER, accumulating sum
      *   SUM of (I + J) for I=1..2, J=1..3
      *   = (1+1)+(1+2)+(1+3)+(2+1)+(2+2)+(2+3)
      *   = 2+3+4+3+4+5 = 21
           MOVE 0 TO WS-TOTAL.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 2
               AFTER WS-J FROM 1 BY 1
               UNTIL WS-J > 3
               COMPUTE WS-PRODUCT = WS-I + WS-J
               ADD WS-PRODUCT TO WS-TOTAL
           END-PERFORM.
           IF WS-TOTAL = 21
               DISPLAY "NC234A-TEST-2 PASS"
           ELSE
               DISPLAY "NC234A-TEST-2 FAIL"
               DISPLAY "  Expected 21, got " WS-TOTAL
           END-IF.
      * Test 3: AFTER with step of 2 in inner loop
      *   I=1..2 (2 iters), J=1,3,5 (3 iters) => 2*3 = 6
           MOVE 0 TO WS-TOTAL.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 2
               AFTER WS-J FROM 1 BY 2
               UNTIL WS-J > 5
               ADD 1 TO WS-TOTAL
           END-PERFORM.
           IF WS-TOTAL = 6
               DISPLAY "NC234A-TEST-3 PASS"
           ELSE
               DISPLAY "NC234A-TEST-3 FAIL"
               DISPLAY "  Expected 6, got " WS-TOTAL
           END-IF.
           STOP RUN.
