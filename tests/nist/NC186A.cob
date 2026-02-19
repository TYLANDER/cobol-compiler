       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC186A.
      *
      * NIST CCVS-style test: OCCURS with subscripts in arithmetic
      * Tests using subscripted OCCURS items as operands in
      * ADD, SUBTRACT, and GIVING clauses.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
           05 WS-VAL PIC 9(4) OCCURS 5 TIMES.
       01 WS-SUM             PIC 9(6) VALUE ZEROS.
       01 WS-DIFF            PIC S9(6) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Initialize table values
           MOVE 100 TO WS-VAL(1).
           MOVE 200 TO WS-VAL(2).
           MOVE 300 TO WS-VAL(3).
           MOVE 400 TO WS-VAL(4).
           MOVE 500 TO WS-VAL(5).
      * Test 1: ADD subscripted items GIVING result
      *   WS-VAL(1) + WS-VAL(2) = 100 + 200 = 300
           MOVE 0 TO WS-SUM.
           ADD WS-VAL(1) WS-VAL(2) GIVING WS-SUM.
           IF WS-SUM = 300
               DISPLAY "NC186A-TEST-1 PASS"
           ELSE
               DISPLAY "NC186A-TEST-1 FAIL"
               DISPLAY "  Expected 300, got " WS-SUM
           END-IF.
      * Test 2: ADD three subscripted items GIVING result
      *   WS-VAL(3) + WS-VAL(4) + WS-VAL(5)
      *   = 300 + 400 + 500 = 1200
           MOVE 0 TO WS-SUM.
           ADD WS-VAL(3) WS-VAL(4) WS-VAL(5)
               GIVING WS-SUM.
           IF WS-SUM = 1200
               DISPLAY "NC186A-TEST-2 PASS"
           ELSE
               DISPLAY "NC186A-TEST-2 FAIL"
               DISPLAY "  Expected 1200, got " WS-SUM
           END-IF.
      * Test 3: SUBTRACT subscripted items
      *   WS-VAL(5) - WS-VAL(2) = 500 - 200 = 300
           MOVE 0 TO WS-DIFF.
           SUBTRACT WS-VAL(2) FROM WS-VAL(5)
               GIVING WS-DIFF.
           IF WS-DIFF = 300
               DISPLAY "NC186A-TEST-3 PASS"
           ELSE
               DISPLAY "NC186A-TEST-3 FAIL"
               DISPLAY "  Expected 300, got " WS-DIFF
           END-IF.
           STOP RUN.
