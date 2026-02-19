       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC150A.
      *
      * NIST CCVS-style test: ON SIZE ERROR for ADD/SUBTRACT
      * Tests the ON SIZE ERROR and NOT ON SIZE ERROR clauses
      * with ADD and SUBTRACT statements.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(4) VALUE ZEROS.
       01 WS-B            PIC 9(4) VALUE ZEROS.
       01 WS-C            PIC 9(4) VALUE ZEROS.
       01 WS-FLAG          PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: ADD ON SIZE ERROR
      *   9999 + 1 overflows PIC 9(4) => SIZE ERROR triggered
           MOVE 9999 TO WS-A.
           MOVE 1 TO WS-B.
           MOVE 0 TO WS-C.
           MOVE 0 TO WS-FLAG.
           ADD WS-A WS-B GIVING WS-C
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 0 TO WS-FLAG
           END-ADD.
           IF WS-FLAG = 1
               DISPLAY "NC150A-TEST-1 PASS"
           ELSE
               DISPLAY "NC150A-TEST-1 FAIL"
               DISPLAY "  Expected SIZE ERROR, flag=" WS-FLAG
               DISPLAY "  C=" WS-C
           END-IF.
      * Test 2: SUBTRACT NOT ON SIZE ERROR
      *   100 - 50 = 50, fits in PIC 9(4) => no error
           MOVE 100 TO WS-A.
           MOVE 50 TO WS-B.
           MOVE 0 TO WS-C.
           MOVE 0 TO WS-FLAG.
           SUBTRACT WS-B FROM WS-A GIVING WS-C
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-SUBTRACT.
           IF WS-FLAG = 2 AND WS-C = 50
               DISPLAY "NC150A-TEST-2 PASS"
           ELSE
               DISPLAY "NC150A-TEST-2 FAIL"
               DISPLAY "  Expected flag=2 C=50, got flag="
                   WS-FLAG " C=" WS-C
           END-IF.
           STOP RUN.
