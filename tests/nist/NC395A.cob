       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC395A.
      *
      * NIST CCVS-style test: Complex IF with AND/OR/NOT
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A               PIC 9(3) VALUE 0.
       01 WS-B               PIC 9(3) VALUE 0.
       01 WS-C               PIC 9(3) VALUE 0.
       01 WS-RESULT          PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: (A > 5 AND B > 5) OR C > 50
      * A=3, B=8, C=60 => false AND true = false, OR true = TRUE
           MOVE 3 TO WS-A.
           MOVE 8 TO WS-B.
           MOVE 60 TO WS-C.
           IF (WS-A > 5 AND WS-B > 5) OR WS-C > 50
               DISPLAY "NC395A-TEST-1 PASS"
           ELSE
               DISPLAY "NC395A-TEST-1 FAIL"
           END-IF.
      * Test 2: NOT (A = 0) AND (B = 0 OR C = 0)
      * A=1, B=0, C=5 => NOT false = true AND (true OR false)
      * = true AND true = TRUE
           MOVE 1 TO WS-A.
           MOVE 0 TO WS-B.
           MOVE 5 TO WS-C.
           IF NOT (WS-A = 0) AND (WS-B = 0 OR WS-C = 0)
               DISPLAY "NC395A-TEST-2 PASS"
           ELSE
               DISPLAY "NC395A-TEST-2 FAIL"
           END-IF.
      * Test 3: NOT (A > 10 OR B > 10)
      * A=5, B=7 => NOT (false OR false) = NOT false = TRUE
           MOVE 5 TO WS-A.
           MOVE 7 TO WS-B.
           IF NOT (WS-A > 10 OR WS-B > 10)
               DISPLAY "NC395A-TEST-3 PASS"
           ELSE
               DISPLAY "NC395A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
