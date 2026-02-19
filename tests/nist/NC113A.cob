       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC113A.
      *
      * NIST CCVS-style test: COMPUTE statement
      * Tests COMPUTE with addition, subtraction,
      * multiplication, parenthesized expressions,
      * and mixed operations (order of operations).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC 9(4) VALUE ZEROS.
       01 WS-B        PIC 9(4) VALUE ZEROS.
       01 WS-C        PIC 9(4) VALUE ZEROS.
       01 WS-RESULT   PIC 9(8) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: COMPUTE with addition and subtraction
           MOVE 100 TO WS-A.
           MOVE 50 TO WS-B.
           COMPUTE WS-RESULT = WS-A + WS-B - 25.
           IF WS-RESULT = 125
               DISPLAY "NC113A-TEST-1 PASS"
           ELSE
               DISPLAY "NC113A-TEST-1 FAIL"
               DISPLAY "  Expected 125, got " WS-RESULT
           END-IF.
      * Test 2: COMPUTE with multiplication
           MOVE 12 TO WS-A.
           MOVE 10 TO WS-B.
           COMPUTE WS-RESULT = WS-A * WS-B.
           IF WS-RESULT = 120
               DISPLAY "NC113A-TEST-2 PASS"
           ELSE
               DISPLAY "NC113A-TEST-2 FAIL"
               DISPLAY "  Expected 120, got " WS-RESULT
           END-IF.
      * Test 3: COMPUTE with parenthesized expressions
           MOVE 10 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE 3 TO WS-C.
           COMPUTE WS-RESULT = (WS-A + WS-B) * WS-C.
           IF WS-RESULT = 45
               DISPLAY "NC113A-TEST-3 PASS"
           ELSE
               DISPLAY "NC113A-TEST-3 FAIL"
               DISPLAY "  Expected 45, got " WS-RESULT
           END-IF.
      * Test 4: COMPUTE order of operations (multiply before add)
           MOVE 10 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE 3 TO WS-C.
           COMPUTE WS-RESULT = WS-A + WS-B * WS-C.
           IF WS-RESULT = 25
               DISPLAY "NC113A-TEST-4 PASS"
           ELSE
               DISPLAY "NC113A-TEST-4 FAIL"
               DISPLAY "  Expected 25, got " WS-RESULT
           END-IF.
           STOP RUN.
