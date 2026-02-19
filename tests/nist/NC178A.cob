       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC178A.
      *
      * NIST CCVS-style test: Multiple ADD/SUBTRACT operands
      * Tests ADD with multiple identifiers in one statement
      * (ADD A B C TO D) and SUBTRACT with multiple operands.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A             PIC 9(4) VALUE ZEROS.
       01 WS-B             PIC 9(4) VALUE ZEROS.
       01 WS-C             PIC 9(4) VALUE ZEROS.
       01 WS-D             PIC 9(4) VALUE ZEROS.
       01 WS-RESULT        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: ADD A B C TO D
      *   A=10, B=20, C=30, D=100
      *   D = D + A + B + C = 100 + 10 + 20 + 30 = 160
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 30 TO WS-C.
           MOVE 100 TO WS-D.
           ADD WS-A WS-B WS-C TO WS-D.
           IF WS-D = 160
               DISPLAY "NC178A-TEST-1 PASS"
           ELSE
               DISPLAY "NC178A-TEST-1 FAIL"
               DISPLAY "  Expected 160 got " WS-D
           END-IF.
      * Test 2: ADD A B C GIVING RESULT
      *   A=5, B=15, C=25 => RESULT = 5+15+25 = 45
           MOVE 5 TO WS-A.
           MOVE 15 TO WS-B.
           MOVE 25 TO WS-C.
           ADD WS-A WS-B WS-C GIVING WS-RESULT.
           IF WS-RESULT = 45
               DISPLAY "NC178A-TEST-2 PASS"
           ELSE
               DISPLAY "NC178A-TEST-2 FAIL"
               DISPLAY "  Expected 45 got " WS-RESULT
           END-IF.
      * Test 3: SUBTRACT A B FROM C
      *   A=3, B=7, C=100 => C = 100 - 3 - 7 = 90
           MOVE 3 TO WS-A.
           MOVE 7 TO WS-B.
           MOVE 100 TO WS-C.
           SUBTRACT WS-A WS-B FROM WS-C.
           IF WS-C = 90
               DISPLAY "NC178A-TEST-3 PASS"
           ELSE
               DISPLAY "NC178A-TEST-3 FAIL"
               DISPLAY "  Expected 90 got " WS-C
           END-IF.
           STOP RUN.
