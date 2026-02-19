       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC156A.
      *
      * NIST CCVS-style test: Complex IF with nested AND/OR/parens
      * Tests compound IF conditions with AND, OR, parentheses,
      * and NOT to verify correct logical evaluation.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A             PIC 9(4) VALUE ZEROS.
       01 WS-B             PIC 9(4) VALUE ZEROS.
       01 WS-C             PIC 9(4) VALUE ZEROS.
       01 WS-FLAG          PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: AND with OR and parentheses
      *   A=5, B=10, C=20
      *   (A < 10 AND B = 10) OR C = 99
      *   => (TRUE AND TRUE) OR FALSE => TRUE OR FALSE => TRUE
           MOVE 5 TO WS-A.
           MOVE 10 TO WS-B.
           MOVE 20 TO WS-C.
           MOVE 0 TO WS-FLAG.
           IF (WS-A < 10 AND WS-B = 10) OR WS-C = 99
               MOVE 1 TO WS-FLAG
           END-IF.
           IF WS-FLAG = 1
               DISPLAY "NC156A-TEST-1 PASS"
           ELSE
               DISPLAY "NC156A-TEST-1 FAIL"
               DISPLAY "  Expected FLAG=1, got " WS-FLAG
           END-IF.
      * Test 2: Parentheses change evaluation order
      *   A=5, B=10, C=20
      *   A < 10 AND (B = 99 OR C = 20)
      *   => TRUE AND (FALSE OR TRUE) => TRUE AND TRUE => TRUE
      *   Without parens: A < 10 AND B = 99 OR C = 20
      *   => (TRUE AND FALSE) OR TRUE => FALSE OR TRUE => TRUE
      *   Both are true here; test with different values:
      *   A=50, B=99, C=20
      *   A < 10 AND (B = 99 OR C = 20)
      *   => FALSE AND (TRUE OR TRUE) => FALSE
      *   A < 10 AND B = 99 OR C = 20
      *   => (FALSE AND TRUE) OR TRUE => TRUE (without parens)
           MOVE 50 TO WS-A.
           MOVE 99 TO WS-B.
           MOVE 20 TO WS-C.
           MOVE 0 TO WS-FLAG.
           IF WS-A < 10 AND (WS-B = 99 OR WS-C = 20)
               MOVE 1 TO WS-FLAG
           ELSE
               MOVE 2 TO WS-FLAG
           END-IF.
           IF WS-FLAG = 2
               DISPLAY "NC156A-TEST-2 PASS"
           ELSE
               DISPLAY "NC156A-TEST-2 FAIL"
               DISPLAY "  Expected FLAG=2, got " WS-FLAG
           END-IF.
      * Test 3: NOT with compound conditions
      *   A=5, B=10
      *   NOT (A > 10 OR B > 20) => NOT (FALSE OR FALSE)
      *   => NOT FALSE => TRUE
           MOVE 5 TO WS-A.
           MOVE 10 TO WS-B.
           MOVE 0 TO WS-FLAG.
           IF NOT (WS-A > 10 OR WS-B > 20)
               MOVE 1 TO WS-FLAG
           END-IF.
           IF WS-FLAG = 1
               DISPLAY "NC156A-TEST-3 PASS"
           ELSE
               DISPLAY "NC156A-TEST-3 FAIL"
               DISPLAY "  Expected FLAG=1, got " WS-FLAG
           END-IF.
           STOP RUN.
