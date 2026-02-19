       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC258A.
      *
      * NIST CCVS-style test: EVALUATE TRUE with complex WHEN
      * Tests EVALUATE TRUE with AND/OR in WHEN conditions.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC 9(4) VALUE ZEROS.
       01 WS-B        PIC 9(4) VALUE ZEROS.
       01 WS-RESULT   PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC258A-CONTROL.
           PERFORM NC258A-TEST-1.
           PERFORM NC258A-TEST-2.
           PERFORM NC258A-TEST-3.
           STOP RUN.
       NC258A-TEST-1.
      * EVALUATE TRUE with ALSO TRUE
      *   A=10, B=20 => first WHEN (A > 5 ALSO B > 15) matches
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 0 TO WS-RESULT.
           EVALUATE TRUE ALSO TRUE
               WHEN WS-A > 5 ALSO WS-B > 15
                   MOVE 1 TO WS-RESULT
               WHEN WS-A > 5 ALSO WS-B < 15
                   MOVE 2 TO WS-RESULT
               WHEN OTHER
                   MOVE 9 TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = 1
               DISPLAY "NC258A-TEST-1 PASS"
           ELSE
               DISPLAY "NC258A-TEST-1 FAIL"
               DISPLAY "  Expected 1, got " WS-RESULT
           END-IF.
       NC258A-TEST-2.
      * EVALUATE TRUE with compound condition in WHEN
      *   A=50, B=3 => WHEN A > 40 AND B < 10 matches
           MOVE 50 TO WS-A.
           MOVE 3 TO WS-B.
           MOVE 0 TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-A > 100
                   MOVE 1 TO WS-RESULT
               WHEN WS-A > 40 AND WS-B < 10
                   MOVE 2 TO WS-RESULT
               WHEN OTHER
                   MOVE 9 TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = 2
               DISPLAY "NC258A-TEST-2 PASS"
           ELSE
               DISPLAY "NC258A-TEST-2 FAIL"
               DISPLAY "  Expected 2, got " WS-RESULT
           END-IF.
       NC258A-TEST-3.
      * EVALUATE TRUE with WHEN OTHER fallthrough
      *   A=1, B=1 => no specific WHEN matches, falls to OTHER
           MOVE 1 TO WS-A.
           MOVE 1 TO WS-B.
           MOVE 0 TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-A > 50 AND WS-B > 50
                   MOVE 1 TO WS-RESULT
               WHEN WS-A > 10 OR WS-B > 10
                   MOVE 2 TO WS-RESULT
               WHEN OTHER
                   MOVE 3 TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = 3
               DISPLAY "NC258A-TEST-3 PASS"
           ELSE
               DISPLAY "NC258A-TEST-3 FAIL"
               DISPLAY "  Expected 3, got " WS-RESULT
           END-IF.
