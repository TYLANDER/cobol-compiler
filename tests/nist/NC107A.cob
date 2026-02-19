       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC107A.
      *
      * NIST CCVS-style test: SUBTRACT variations
      * Tests SUBTRACT FROM, SUBTRACT FROM GIVING,
      * and multiple operands.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC 9(4) VALUE ZEROS.
       01 WS-B PIC 9(4) VALUE ZEROS.
       01 WS-C PIC 9(4) VALUE ZEROS.
       01 WS-RESULT PIC 9(8) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: SUBTRACT FROM (accumulator)
           MOVE 1000 TO WS-A.
           MOVE 0300 TO WS-B.
           SUBTRACT WS-B FROM WS-A.
           IF WS-A = 700
               DISPLAY "NC107A-TEST-1 PASS"
           ELSE
               DISPLAY "NC107A-TEST-1 FAIL"
           END-IF.
      * Test 2: SUBTRACT FROM GIVING
           MOVE 5000 TO WS-A.
           MOVE 1234 TO WS-B.
           SUBTRACT WS-B FROM WS-A GIVING WS-RESULT.
           IF WS-RESULT = 3766
               DISPLAY "NC107A-TEST-2 PASS"
           ELSE
               DISPLAY "NC107A-TEST-2 FAIL"
           END-IF.
      * Test 3: SUBTRACT yielding zero
           MOVE 4242 TO WS-A.
           MOVE 4242 TO WS-B.
           SUBTRACT WS-B FROM WS-A GIVING WS-RESULT.
           IF WS-RESULT = 0
               DISPLAY "NC107A-TEST-3 PASS"
           ELSE
               DISPLAY "NC107A-TEST-3 FAIL"
           END-IF.
      * Test 4: Chained subtracts
           MOVE 1000 TO WS-RESULT.
           MOVE 0100 TO WS-A.
           SUBTRACT WS-A FROM WS-RESULT.
           SUBTRACT WS-A FROM WS-RESULT.
           SUBTRACT WS-A FROM WS-RESULT.
           IF WS-RESULT = 700
               DISPLAY "NC107A-TEST-4 PASS"
           ELSE
               DISPLAY "NC107A-TEST-4 FAIL"
           END-IF.
           STOP RUN.
