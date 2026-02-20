       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC382A.
      *
      * NIST CCVS-style test: COMPUTE with complex expressions
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A               PIC 9(5) VALUE 0.
       01 WS-B               PIC 9(5) VALUE 0.
       01 WS-C               PIC 9(5) VALUE 0.
       01 WS-RESULT          PIC 9(5) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: COMPUTE with addition and multiplication
      * (2 + 3) * 4 = 20
           MOVE 2 TO WS-A.
           MOVE 3 TO WS-B.
           MOVE 4 TO WS-C.
           COMPUTE WS-RESULT = (WS-A + WS-B) * WS-C.
           IF WS-RESULT = 20
               DISPLAY "NC382A-TEST-1 PASS"
           ELSE
               DISPLAY "NC382A-TEST-1 FAIL"
               DISPLAY "  Expected 20, got " WS-RESULT
           END-IF.
      * Test 2: COMPUTE with subtraction and nested parens
      * (10 - (3 + 2)) * 2 = 10
           MOVE 10 TO WS-A.
           MOVE 3 TO WS-B.
           MOVE 2 TO WS-C.
           COMPUTE WS-RESULT =
               (WS-A - (WS-B + WS-C)) * WS-C.
           IF WS-RESULT = 10
               DISPLAY "NC382A-TEST-2 PASS"
           ELSE
               DISPLAY "NC382A-TEST-2 FAIL"
               DISPLAY "  Expected 10, got " WS-RESULT
           END-IF.
      * Test 3: COMPUTE with multiple operators
      * 5 * 6 + 3 * 2 - 1 = 35
           MOVE 5 TO WS-A.
           MOVE 6 TO WS-B.
           MOVE 3 TO WS-C.
           COMPUTE WS-RESULT =
               WS-A * WS-B + WS-C * 2 - 1.
           IF WS-RESULT = 35
               DISPLAY "NC382A-TEST-3 PASS"
           ELSE
               DISPLAY "NC382A-TEST-3 FAIL"
               DISPLAY "  Expected 35, got " WS-RESULT
           END-IF.
           STOP RUN.
