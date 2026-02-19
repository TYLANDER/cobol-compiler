       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC302A.
      *
      * NIST CCVS-style test: Signed arithmetic with PIC S9
      * Tests negative values produced by SUBTRACT and ADD,
      * verifying results by comparing against zero.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC S9(4) VALUE ZEROS.
       01 WS-B        PIC S9(4) VALUE ZEROS.
       01 WS-C        PIC S9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: SUBTRACT producing a negative result
      *   5 - 15 = -10; result should be < 0
           MOVE 5 TO WS-A.
           MOVE 15 TO WS-B.
           SUBTRACT WS-B FROM WS-A.
           IF WS-A < 0
               DISPLAY "NC302A-TEST-1 PASS"
           ELSE
               DISPLAY "NC302A-TEST-1 FAIL"
               DISPLAY "  Expected negative, got " WS-A
           END-IF.
      * Test 2: ADD after SUBTRACT, result still negative
      *   Start: A=10, B=50 => A - B = -40
      *   Then ADD 30 TO A => -40 + 30 = -10, still < 0
           MOVE 10 TO WS-A.
           MOVE 50 TO WS-B.
           SUBTRACT WS-B FROM WS-A.
           ADD 30 TO WS-A.
           IF WS-A < 0
               DISPLAY "NC302A-TEST-2 PASS"
           ELSE
               DISPLAY "NC302A-TEST-2 FAIL"
               DISPLAY "  Expected negative, got " WS-A
           END-IF.
      * Test 3: Negative + positive becomes positive
      *   A=10-50=-40, then ADD 100 TO A => 60
      *   Result should be > 0
           MOVE 10 TO WS-A.
           MOVE 50 TO WS-B.
           SUBTRACT WS-B FROM WS-A.
           ADD 100 TO WS-A.
           IF WS-A > 0
               DISPLAY "NC302A-TEST-3 PASS"
           ELSE
               DISPLAY "NC302A-TEST-3 FAIL"
               DISPLAY "  Expected positive, got " WS-A
           END-IF.
           STOP RUN.
