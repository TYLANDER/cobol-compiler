       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC309A.
      *
      * NIST CCVS-style test: ADD/SUBTRACT with multiple operands
      * Tests ADD A B C TO D (sum of all) and SUBTRACT with
      * multiple identifiers, verifying final accumulator values.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC 9(4) VALUE ZEROS.
       01 WS-B        PIC 9(4) VALUE ZEROS.
       01 WS-C        PIC 9(4) VALUE ZEROS.
       01 WS-D        PIC 9(4) VALUE ZEROS.
       01 WS-E        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: ADD A B C TO D
      *   D = 1000 + 10 + 20 + 30 = 1060
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 30 TO WS-C.
           MOVE 1000 TO WS-D.
           ADD WS-A WS-B WS-C TO WS-D.
           IF WS-D = 1060
               DISPLAY "NC309A-TEST-1 PASS"
           ELSE
               DISPLAY "NC309A-TEST-1 FAIL"
               DISPLAY "  Expected 1060, got " WS-D
           END-IF.
      * Test 2: SUBTRACT A B FROM E
      *   E = 500 - 100 - 200 = 200
           MOVE 100 TO WS-A.
           MOVE 200 TO WS-B.
           MOVE 500 TO WS-E.
           SUBTRACT WS-A WS-B FROM WS-E.
           IF WS-E = 200
               DISPLAY "NC309A-TEST-2 PASS"
           ELSE
               DISPLAY "NC309A-TEST-2 FAIL"
               DISPLAY "  Expected 200, got " WS-E
           END-IF.
      * Test 3: ADD A B C GIVING D (D replaced, not accumulated)
      *   D = 5 + 15 + 25 = 45 (D's original value irrelevant)
           MOVE 5 TO WS-A.
           MOVE 15 TO WS-B.
           MOVE 25 TO WS-C.
           MOVE 9999 TO WS-D.
           ADD WS-A WS-B WS-C GIVING WS-D.
           IF WS-D = 45
               DISPLAY "NC309A-TEST-3 PASS"
           ELSE
               DISPLAY "NC309A-TEST-3 FAIL"
               DISPLAY "  Expected 45, got " WS-D
           END-IF.
           STOP RUN.
