       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC330A.
      *
      * NIST CCVS-style test: DIVIDE BY ZERO with ON SIZE ERROR
      * Tests that dividing by zero triggers the SIZE ERROR handler.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DIVIDEND     PIC 9(4) VALUE 0.
       01 WS-DIVISOR      PIC 9(4) VALUE 0.
       01 WS-QUOTIENT     PIC 9(4) VALUE 0.
       01 WS-REMAINDER    PIC 9(4) VALUE 0.
       01 WS-FLAG          PIC 9(1) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: DIVIDE BY ZERO triggers ON SIZE ERROR
      *   Dividing 100 by 0 should trigger SIZE ERROR.
           MOVE 100 TO WS-DIVIDEND.
           MOVE 0 TO WS-DIVISOR.
           MOVE 0 TO WS-FLAG.
           DIVIDE WS-DIVIDEND BY WS-DIVISOR
               GIVING WS-QUOTIENT
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-DIVIDE.
           IF WS-FLAG = 1
               DISPLAY "NC330A-TEST-1 PASS"
           ELSE
               DISPLAY "NC330A-TEST-1 FAIL"
               DISPLAY "  Expected SIZE ERROR, flag=" WS-FLAG
           END-IF.
      * Test 2: Normal DIVIDE does NOT trigger SIZE ERROR
      *   Dividing 100 by 5 should give 20, not trigger SIZE ERROR.
           MOVE 100 TO WS-DIVIDEND.
           MOVE 5 TO WS-DIVISOR.
           MOVE 0 TO WS-FLAG.
           MOVE 0 TO WS-QUOTIENT.
           DIVIDE WS-DIVIDEND BY WS-DIVISOR
               GIVING WS-QUOTIENT
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-DIVIDE.
           IF WS-FLAG = 2 AND WS-QUOTIENT = 20
               DISPLAY "NC330A-TEST-2 PASS"
           ELSE
               DISPLAY "NC330A-TEST-2 FAIL"
               DISPLAY "  Expected flag=2 quot=20, got flag="
                   WS-FLAG " quot=" WS-QUOTIENT
           END-IF.
      * Test 3: DIVIDE with REMAINDER and SIZE ERROR on zero
      *   DIVIDE 50 BY 0 GIVING Q REMAINDER R => SIZE ERROR
           MOVE 50 TO WS-DIVIDEND.
           MOVE 0 TO WS-DIVISOR.
           MOVE 0 TO WS-FLAG.
           MOVE 99 TO WS-QUOTIENT.
           MOVE 99 TO WS-REMAINDER.
           DIVIDE WS-DIVIDEND BY WS-DIVISOR
               GIVING WS-QUOTIENT REMAINDER WS-REMAINDER
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-DIVIDE.
           IF WS-FLAG = 1
               DISPLAY "NC330A-TEST-3 PASS"
           ELSE
               DISPLAY "NC330A-TEST-3 FAIL"
               DISPLAY "  Expected SIZE ERROR, flag=" WS-FLAG
           END-IF.
           STOP RUN.
