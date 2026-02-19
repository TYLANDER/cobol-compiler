       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC102A.
      *
      * NIST CCVS-style test: Arithmetic (ADD, SUBTRACT)
      * Tests basic arithmetic operations.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC 9(4) VALUE ZEROS.
       01 WS-B PIC 9(4) VALUE ZEROS.
       01 WS-RESULT PIC 9(8) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: ADD two values with GIVING
           MOVE 1234 TO WS-A.
           MOVE 5678 TO WS-B.
           ADD WS-A WS-B GIVING WS-RESULT.
           IF WS-RESULT = 6912
               DISPLAY "NC102A-TEST-1 PASS"
           ELSE
               DISPLAY "NC102A-TEST-1 FAIL"
           END-IF.
      * Test 2: SUBTRACT with GIVING
           MOVE 5000 TO WS-A.
           MOVE 3000 TO WS-B.
           SUBTRACT WS-B FROM WS-A GIVING WS-RESULT.
           IF WS-RESULT = 2000
               DISPLAY "NC102A-TEST-2 PASS"
           ELSE
               DISPLAY "NC102A-TEST-2 FAIL"
           END-IF.
      * Test 3: ADD TO (accumulator form)
           MOVE 0 TO WS-RESULT.
           MOVE 100 TO WS-A.
           ADD WS-A TO WS-RESULT.
           ADD WS-A TO WS-RESULT.
           ADD WS-A TO WS-RESULT.
           IF WS-RESULT = 300
               DISPLAY "NC102A-TEST-3 PASS"
           ELSE
               DISPLAY "NC102A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
