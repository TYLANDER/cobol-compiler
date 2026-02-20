       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC219A.
      *
      * NIST CCVS-style test: MULTIPLE CALL TO SAME SUBPROGRAM
      * Tests calling the same subprogram multiple times with
      * different parameters.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-INPUT-VAL          PIC 9(4)  VALUE 0.
       01  WS-MULTIPLIER         PIC 9(4)  VALUE 0.
       01  WS-OUTPUT-VAL         PIC 9(8)  VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: 25 * 4 = 100
           MOVE 25 TO WS-INPUT-VAL.
           MOVE 4 TO WS-MULTIPLIER.
           MOVE 0 TO WS-OUTPUT-VAL.
           CALL "IC219A-SUB" USING WS-INPUT-VAL
                                    WS-MULTIPLIER
                                    WS-OUTPUT-VAL.
           IF WS-OUTPUT-VAL = 100
               DISPLAY "IC219A-TEST-1 PASS"
           ELSE
               DISPLAY "IC219A-TEST-1 FAIL"
               DISPLAY "  Expected 100, got " WS-OUTPUT-VAL
           END-IF.
      * Test 2: 150 * 3 = 450
           MOVE 150 TO WS-INPUT-VAL.
           MOVE 3 TO WS-MULTIPLIER.
           MOVE 0 TO WS-OUTPUT-VAL.
           CALL "IC219A-SUB" USING WS-INPUT-VAL
                                    WS-MULTIPLIER
                                    WS-OUTPUT-VAL.
           IF WS-OUTPUT-VAL = 450
               DISPLAY "IC219A-TEST-2 PASS"
           ELSE
               DISPLAY "IC219A-TEST-2 FAIL"
               DISPLAY "  Expected 450, got " WS-OUTPUT-VAL
           END-IF.
      * Test 3: 500 * 10 = 5000
           MOVE 500 TO WS-INPUT-VAL.
           MOVE 10 TO WS-MULTIPLIER.
           MOVE 0 TO WS-OUTPUT-VAL.
           CALL "IC219A-SUB" USING WS-INPUT-VAL
                                    WS-MULTIPLIER
                                    WS-OUTPUT-VAL.
           IF WS-OUTPUT-VAL = 5000
               DISPLAY "IC219A-TEST-3 PASS"
           ELSE
               DISPLAY "IC219A-TEST-3 FAIL"
               DISPLAY "  Expected 5000, got " WS-OUTPUT-VAL
           END-IF.
           STOP RUN.
