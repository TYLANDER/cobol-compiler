       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC215A.
      *
      * NIST CCVS-style test: ADD/SUBTRACT with ON SIZE ERROR
      * Tests that ON SIZE ERROR fires when result overflows
      * the target PIC, and NOT ON SIZE ERROR fires otherwise.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(3)  VALUE ZEROS.
       01 WS-B            PIC 9(3)  VALUE ZEROS.
       01 WS-RESULT       PIC 9(3)  VALUE ZEROS.
       01 WS-FLAG         PIC 9     VALUE 0.
       01 WS-BIG          PIC 9(3)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: ADD without overflow - NOT ON SIZE ERROR
      *   100 + 200 = 300, fits in PIC 9(3)
           MOVE 100 TO WS-A.
           MOVE 200 TO WS-B.
           MOVE 0 TO WS-RESULT.
           MOVE 0 TO WS-FLAG.
           ADD WS-A WS-B GIVING WS-RESULT
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-ADD.
           IF WS-RESULT = 300 AND WS-FLAG = 2
               DISPLAY "NC215A-TEST-1 PASS"
           ELSE
               DISPLAY "NC215A-TEST-1 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
                   " FLAG=" WS-FLAG
           END-IF.
      * Test 2: ADD with overflow - ON SIZE ERROR
      *   600 + 500 = 1100, does not fit in PIC 9(3)
           MOVE 600 TO WS-A.
           MOVE 500 TO WS-B.
           MOVE 0 TO WS-RESULT.
           MOVE 0 TO WS-FLAG.
           ADD WS-A WS-B GIVING WS-RESULT
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-ADD.
           IF WS-FLAG = 1
               DISPLAY "NC215A-TEST-2 PASS"
           ELSE
               DISPLAY "NC215A-TEST-2 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
                   " FLAG=" WS-FLAG
           END-IF.
      * Test 3: SUBTRACT with ON SIZE ERROR
      *   100 - 200 would give -100, unsigned PIC 9(3) overflows
           MOVE 100 TO WS-A.
           MOVE 200 TO WS-B.
           MOVE 0 TO WS-RESULT.
           MOVE 0 TO WS-FLAG.
           SUBTRACT WS-B FROM WS-A GIVING WS-RESULT
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-SUBTRACT.
           IF WS-FLAG = 1
               DISPLAY "NC215A-TEST-3 PASS"
           ELSE
               DISPLAY "NC215A-TEST-3 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
                   " FLAG=" WS-FLAG
           END-IF.
           STOP RUN.
