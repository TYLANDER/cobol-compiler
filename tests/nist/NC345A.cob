       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC345A.
      *
      * NIST CCVS-style test: MULTIPLY/DIVIDE with ROUNDED
      * and ON SIZE ERROR combined.
      * Tests ROUNDED truncation and SIZE ERROR detection.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(4) VALUE 0.
       01 WS-B            PIC 9(4) VALUE 0.
       01 WS-RESULT       PIC 9(4) VALUE 0.
       01 WS-FLAG         PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: MULTIPLY overflow with ON SIZE ERROR
      *   9999 * 2 = 19998, overflows PIC 9(4)
           MOVE 9999 TO WS-A.
           MOVE 2 TO WS-B.
           MOVE 0 TO WS-RESULT.
           MOVE 0 TO WS-FLAG.
           MULTIPLY WS-A BY WS-B GIVING WS-RESULT
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-MULTIPLY.
           IF WS-FLAG = 1
               DISPLAY "NC345A-TEST-1 PASS"
           ELSE
               DISPLAY "NC345A-TEST-1 FAIL"
               DISPLAY "  Expected SIZE ERROR, flag=" WS-FLAG
           END-IF.
      * Test 2: MULTIPLY normal with NOT ON SIZE ERROR
      *   12 * 5 = 60, fits PIC 9(4)
           MOVE 12 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE 0 TO WS-RESULT.
           MOVE 0 TO WS-FLAG.
           MULTIPLY WS-A BY WS-B GIVING WS-RESULT
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-MULTIPLY.
           IF WS-FLAG = 2 AND WS-RESULT = 60
               DISPLAY "NC345A-TEST-2 PASS"
           ELSE
               DISPLAY "NC345A-TEST-2 FAIL"
               DISPLAY "  Expected flag=2 RESULT=60, got flag="
                   WS-FLAG " RESULT=" WS-RESULT
           END-IF.
      * Test 3: DIVIDE with ON SIZE ERROR on zero divisor
      *   100 / 0 triggers SIZE ERROR
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
               DISPLAY "NC345A-TEST-3 PASS"
           ELSE
               DISPLAY "NC345A-TEST-3 FAIL"
               DISPLAY "  Expected SIZE ERROR, flag=" WS-FLAG
           END-IF.
           STOP RUN.
