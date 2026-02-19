       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC231A.
      *
      * NIST CCVS-style test: MULTIPLY with ON SIZE ERROR
      * Tests overflow detection via ON SIZE ERROR and
      * NOT ON SIZE ERROR branches.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC 9(4) VALUE ZEROS.
       01 WS-B        PIC 9(4) VALUE ZEROS.
       01 WS-RESULT   PIC 9(4) VALUE ZEROS.
       01 WS-FLAG     PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Normal MULTIPLY works correctly
      *   5 * 10 = 50, fits in PIC 9(4)
           MOVE 5 TO WS-A.
           MOVE 10 TO WS-B.
           MOVE 0 TO WS-RESULT.
           MOVE 0 TO WS-FLAG.
           MULTIPLY WS-A BY WS-B GIVING WS-RESULT
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 0 TO WS-FLAG
           END-MULTIPLY.
           IF WS-RESULT = 50 AND WS-FLAG = 0
               DISPLAY "NC231A-TEST-1 PASS"
           ELSE
               DISPLAY "NC231A-TEST-1 FAIL"
               DISPLAY "  Expected 50 flag=0, got "
                   WS-RESULT " flag=" WS-FLAG
           END-IF.
      * Test 2: MULTIPLY that overflows triggers SIZE ERROR
      *   9999 * 9999 overflows PIC 9(4)
           MOVE 9999 TO WS-A.
           MOVE 9999 TO WS-B.
           MOVE 0 TO WS-RESULT.
           MOVE 0 TO WS-FLAG.
           MULTIPLY WS-A BY WS-B GIVING WS-RESULT
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 0 TO WS-FLAG
           END-MULTIPLY.
           IF WS-FLAG = 1
               DISPLAY "NC231A-TEST-2 PASS"
           ELSE
               DISPLAY "NC231A-TEST-2 FAIL"
               DISPLAY "  Expected SIZE ERROR, flag=" WS-FLAG
           END-IF.
      * Test 3: NOT ON SIZE ERROR branch executes for normal
      *   25 * 4 = 100, fits in PIC 9(4)
           MOVE 25 TO WS-A.
           MOVE 4 TO WS-B.
           MOVE 0 TO WS-RESULT.
           MOVE 0 TO WS-FLAG.
           MULTIPLY WS-A BY WS-B GIVING WS-RESULT
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-MULTIPLY.
           IF WS-RESULT = 100 AND WS-FLAG = 2
               DISPLAY "NC231A-TEST-3 PASS"
           ELSE
               DISPLAY "NC231A-TEST-3 FAIL"
               DISPLAY "  Expected 100 flag=2, got "
                   WS-RESULT " flag=" WS-FLAG
           END-IF.
           STOP RUN.
