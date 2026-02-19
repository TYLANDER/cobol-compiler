       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC154A.
      *
      * NIST CCVS-style test: Nested PERFORM VARYING (two levels)
      * Tests nested inline PERFORM VARYING loops, verifying
      * total iteration count and accumulated values.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I             PIC 9(4) VALUE ZEROS.
       01 WS-J             PIC 9(4) VALUE ZEROS.
       01 WS-COUNT         PIC 9(4) VALUE ZEROS.
       01 WS-SUM           PIC 9(4) VALUE ZEROS.
       01 WS-LAST-I        PIC 9(4) VALUE ZEROS.
       01 WS-LAST-J        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Nested PERFORM — outer 1..3, inner 1..4
      *   Outer loop runs 3 times, inner loop runs 4 times each
      *   Total iterations = 3 * 4 = 12
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > 4
                   ADD 1 TO WS-COUNT
               END-PERFORM
           END-PERFORM.
           IF WS-COUNT = 12
               DISPLAY "NC154A-TEST-1 PASS"
           ELSE
               DISPLAY "NC154A-TEST-1 FAIL"
               DISPLAY "  Expected COUNT=12, got " WS-COUNT
           END-IF.
      * Test 2: Nested PERFORM — accumulate I*J product sum
      *   I=1..2, J=1..3
      *   Products: 1*1=1, 1*2=2, 1*3=3, 2*1=2, 2*2=4, 2*3=6
      *   Sum = 1+2+3+2+4+6 = 18
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 2
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > 3
                   COMPUTE WS-SUM = WS-SUM + (WS-I * WS-J)
               END-PERFORM
           END-PERFORM.
           IF WS-SUM = 18
               DISPLAY "NC154A-TEST-2 PASS"
           ELSE
               DISPLAY "NC154A-TEST-2 FAIL"
               DISPLAY "  Expected SUM=18, got " WS-SUM
           END-IF.
      * Test 3: Verify loop variables have correct values after loop
      *   After PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
      *   I should be 5 (the value that caused termination)
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 4
               MOVE WS-I TO WS-LAST-I
           END-PERFORM.
           IF WS-I = 5 AND WS-LAST-I = 4
               DISPLAY "NC154A-TEST-3 PASS"
           ELSE
               DISPLAY "NC154A-TEST-3 FAIL"
               DISPLAY "  Expected I=5 LAST-I=4"
               DISPLAY "  Got I=" WS-I " LAST-I=" WS-LAST-I
           END-IF.
           STOP RUN.
