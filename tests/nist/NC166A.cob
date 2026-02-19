       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC166A.
      *
      * NIST CCVS-style test: DIVIDE ON SIZE ERROR
      * Tests DIVIDE statement with ON SIZE ERROR for divide
      * by zero and NOT ON SIZE ERROR for valid division.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(4) VALUE ZEROS.
       01 WS-B            PIC 9(4) VALUE ZEROS.
       01 WS-RESULT       PIC 9(4) VALUE ZEROS.
       01 WS-REMAIN       PIC 9(4) VALUE ZEROS.
       01 WS-FLAG          PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: DIVIDE by zero triggers ON SIZE ERROR
      *   100 / 0 => SIZE ERROR
           MOVE 100 TO WS-A.
           MOVE 0 TO WS-B.
           MOVE 0 TO WS-RESULT.
           MOVE 0 TO WS-FLAG.
           DIVIDE WS-A BY WS-B GIVING WS-RESULT
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-DIVIDE.
           IF WS-FLAG = 1
               DISPLAY "NC166A-TEST-1 PASS"
           ELSE
               DISPLAY "NC166A-TEST-1 FAIL"
               DISPLAY "  Expected flag=1, got " WS-FLAG
               DISPLAY "  RESULT=" WS-RESULT
           END-IF.
      * Test 2: Valid DIVIDE triggers NOT ON SIZE ERROR
      *   100 / 4 = 25, fits in PIC 9(4)
           MOVE 100 TO WS-A.
           MOVE 4 TO WS-B.
           MOVE 0 TO WS-RESULT.
           MOVE 0 TO WS-FLAG.
           DIVIDE WS-A BY WS-B GIVING WS-RESULT
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-DIVIDE.
           IF WS-FLAG = 2 AND WS-RESULT = 25
               DISPLAY "NC166A-TEST-2 PASS"
           ELSE
               DISPLAY "NC166A-TEST-2 FAIL"
               DISPLAY "  Expected flag=2 RESULT=25"
               DISPLAY "  Got flag=" WS-FLAG
                   " RESULT=" WS-RESULT
           END-IF.
      * Test 3: DIVIDE with REMAINDER
      *   17 / 5 = 3 remainder 2
           MOVE 17 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE 0 TO WS-RESULT.
           MOVE 0 TO WS-REMAIN.
           DIVIDE WS-A BY WS-B GIVING WS-RESULT
               REMAINDER WS-REMAIN.
           IF WS-RESULT = 3 AND WS-REMAIN = 2
               DISPLAY "NC166A-TEST-3 PASS"
           ELSE
               DISPLAY "NC166A-TEST-3 FAIL"
               DISPLAY "  Expected Q=3 R=2"
               DISPLAY "  Got Q=" WS-RESULT " R=" WS-REMAIN
           END-IF.
           STOP RUN.
