       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC132A.
      *
      * NIST CCVS-style test: Nested IF and complex conditions
      * Tests nested IF/ELSE/END-IF, combined AND/OR
      * conditions, and NOT condition.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(4) VALUE ZEROS.
       01 WS-B            PIC 9(4) VALUE ZEROS.
       01 WS-C            PIC 9(4) VALUE ZEROS.
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       01 WS-FLAG         PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Nested IF/ELSE/END-IF
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           IF WS-A > 5
               IF WS-B > 15
                   MOVE "BOTH-HIGH" TO WS-RESULT
               ELSE
                   MOVE "A-ONLY" TO WS-RESULT
               END-IF
           ELSE
               MOVE "NEITHER" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "BOTH-HIGH"
               DISPLAY "NC132A-TEST-1 PASS"
           ELSE
               DISPLAY "NC132A-TEST-1 FAIL"
               DISPLAY "  Expected BOTH-HIGH, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 2: Combined conditions with AND/OR
      *   (A > 5 AND B > 10) OR C = 99
           MOVE 3 TO WS-A.
           MOVE 7 TO WS-B.
           MOVE 99 TO WS-C.
           IF (WS-A > 5 AND WS-B > 10)
               OR WS-C = 99
               DISPLAY "NC132A-TEST-2 PASS"
           ELSE
               DISPLAY "NC132A-TEST-2 FAIL"
               DISPLAY "  A=" WS-A " B=" WS-B
                   " C=" WS-C
           END-IF.
      * Test 3: NOT condition
           MOVE 0 TO WS-FLAG.
           IF NOT WS-FLAG = 1
               DISPLAY "NC132A-TEST-3 PASS"
           ELSE
               DISPLAY "NC132A-TEST-3 FAIL"
               DISPLAY "  FLAG=" WS-FLAG
                   " expected NOT 1"
           END-IF.
           STOP RUN.
