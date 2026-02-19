       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC344A.
      *
      * NIST CCVS-style test: ADD/SUBTRACT with ON SIZE ERROR
      * and NOT ON SIZE ERROR
      * Tests that SIZE ERROR fires on overflow and
      * NOT ON SIZE ERROR fires on normal completion.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(4) VALUE 0.
       01 WS-B            PIC 9(4) VALUE 0.
       01 WS-C            PIC 9(4) VALUE 0.
       01 WS-FLAG         PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: ADD overflow triggers ON SIZE ERROR
      *   9990 + 20 = 10010, overflows PIC 9(4)
           MOVE 9990 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 0 TO WS-C.
           MOVE 0 TO WS-FLAG.
           ADD WS-A WS-B GIVING WS-C
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-ADD.
           IF WS-FLAG = 1
               DISPLAY "NC344A-TEST-1 PASS"
           ELSE
               DISPLAY "NC344A-TEST-1 FAIL"
               DISPLAY "  Expected SIZE ERROR, flag=" WS-FLAG
           END-IF.
      * Test 2: ADD no overflow triggers NOT ON SIZE ERROR
      *   100 + 200 = 300, fits PIC 9(4)
           MOVE 100 TO WS-A.
           MOVE 200 TO WS-B.
           MOVE 0 TO WS-C.
           MOVE 0 TO WS-FLAG.
           ADD WS-A WS-B GIVING WS-C
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-ADD.
           IF WS-FLAG = 2 AND WS-C = 300
               DISPLAY "NC344A-TEST-2 PASS"
           ELSE
               DISPLAY "NC344A-TEST-2 FAIL"
               DISPLAY "  Expected flag=2 C=300, got flag="
                   WS-FLAG " C=" WS-C
           END-IF.
      * Test 3: SUBTRACT overflow triggers ON SIZE ERROR
      *   50 - 100 = -50, unsigned PIC 9(4) overflows
           MOVE 50 TO WS-A.
           MOVE 100 TO WS-B.
           MOVE 0 TO WS-C.
           MOVE 0 TO WS-FLAG.
           SUBTRACT WS-B FROM WS-A GIVING WS-C
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG
           END-SUBTRACT.
           IF WS-FLAG = 1
               DISPLAY "NC344A-TEST-3 PASS"
           ELSE
               DISPLAY "NC344A-TEST-3 FAIL"
               DISPLAY "  Expected SIZE ERROR, flag=" WS-FLAG
           END-IF.
           STOP RUN.
