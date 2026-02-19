       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC227A.
      *
      * NIST CCVS-style test: ADD GIVING and SUBTRACT GIVING
      * with different targets. Tests that GIVING stores the
      * result in the target without modifying the source operands.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A             PIC 9(4) VALUE ZEROS.
       01 WS-B             PIC 9(4) VALUE ZEROS.
       01 WS-C             PIC 9(4) VALUE ZEROS.
       01 WS-D             PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: ADD GIVING - sources unchanged
      *   A=25, B=75 => C = 25 + 75 = 100
      *   A and B should remain 25 and 75
           MOVE 25 TO WS-A.
           MOVE 75 TO WS-B.
           MOVE 0 TO WS-C.
           ADD WS-A WS-B GIVING WS-C.
           IF WS-C = 100
               AND WS-A = 25
               AND WS-B = 75
               DISPLAY "NC227A-TEST-1 PASS"
           ELSE
               DISPLAY "NC227A-TEST-1 FAIL"
               DISPLAY "  A=" WS-A " B=" WS-B " C=" WS-C
           END-IF.
      * Test 2: SUBTRACT GIVING - sources unchanged
      *   A=200, B=80 => D = 200 - 80 = 120
      *   A and B should remain 200 and 80
           MOVE 200 TO WS-A.
           MOVE 80 TO WS-B.
           MOVE 0 TO WS-D.
           SUBTRACT WS-B FROM WS-A GIVING WS-D.
           IF WS-D = 120
               AND WS-A = 200
               AND WS-B = 80
               DISPLAY "NC227A-TEST-2 PASS"
           ELSE
               DISPLAY "NC227A-TEST-2 FAIL"
               DISPLAY "  A=" WS-A " B=" WS-B " D=" WS-D
           END-IF.
      * Test 3: ADD GIVING with three sources
      *   A=10, B=20, C=30 => D = 10 + 20 + 30 = 60
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 30 TO WS-C.
           MOVE 0 TO WS-D.
           ADD WS-A WS-B WS-C GIVING WS-D.
           IF WS-D = 60
               DISPLAY "NC227A-TEST-3 PASS"
           ELSE
               DISPLAY "NC227A-TEST-3 FAIL"
               DISPLAY "  Expected 60, got D=" WS-D
           END-IF.
           STOP RUN.
