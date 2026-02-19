       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC179A.
      *
      * NIST CCVS-style test: Nested IF with AND/OR/NOT
      * Tests complex conditional logic combining nested IF
      * statements with AND, OR, and NOT operators.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A             PIC 9(3) VALUE ZEROS.
       01 WS-B             PIC 9(3) VALUE ZEROS.
       01 WS-C             PIC 9(3) VALUE ZEROS.
       01 WS-RESULT        PIC X(4) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: AND with nested IF
      *   A=10, B=20, C=30
      *   IF A < B AND B < C => TRUE, then inner IF C > 25
      *   => TRUE => RESULT = "PASS"
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 30 TO WS-C.
           MOVE "FAIL" TO WS-RESULT.
           IF WS-A < WS-B AND WS-B < WS-C
               IF WS-C > 25
                   MOVE "PASS" TO WS-RESULT
               END-IF
           END-IF.
           IF WS-RESULT = "PASS"
               DISPLAY "NC179A-TEST-1 PASS"
           ELSE
               DISPLAY "NC179A-TEST-1 FAIL"
               DISPLAY "  Expected PASS got " WS-RESULT
           END-IF.
      * Test 2: OR condition
      *   A=100, B=5, C=200
      *   IF A > 50 OR B > 50 => TRUE (A>50 is true)
           MOVE 100 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE 200 TO WS-C.
           MOVE "FAIL" TO WS-RESULT.
           IF WS-A > 50 OR WS-B > 50
               MOVE "PASS" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "PASS"
               DISPLAY "NC179A-TEST-2 PASS"
           ELSE
               DISPLAY "NC179A-TEST-2 FAIL"
               DISPLAY "  Expected PASS got " WS-RESULT
           END-IF.
      * Test 3: NOT combined with AND
      *   A=10, B=20
      *   IF NOT (A > B) AND NOT (B = 0) => TRUE
      *   (A is not > B, and B is not 0)
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE "FAIL" TO WS-RESULT.
           IF NOT (WS-A > WS-B) AND NOT (WS-B = 0)
               MOVE "PASS" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "PASS"
               DISPLAY "NC179A-TEST-3 PASS"
           ELSE
               DISPLAY "NC179A-TEST-3 FAIL"
               DISPLAY "  Expected PASS got " WS-RESULT
           END-IF.
           STOP RUN.
