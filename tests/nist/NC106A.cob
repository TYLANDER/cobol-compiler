       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC106A.
      *
      * NIST CCVS-style test: Mixed arithmetic with different PIC
      * sizes. Tests that arithmetic works across PIC 9(2), 9(4),
      * and 9(8) fields.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SMALL PIC 9(2) VALUE ZEROS.
       01 WS-MED   PIC 9(4) VALUE ZEROS.
       01 WS-BIG   PIC 9(8) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: ADD small to big
           MOVE 50 TO WS-SMALL.
           MOVE 0 TO WS-BIG.
           ADD WS-SMALL TO WS-BIG.
           IF WS-BIG = 50
               DISPLAY "NC106A-TEST-1 PASS"
           ELSE
               DISPLAY "NC106A-TEST-1 FAIL"
           END-IF.
      * Test 2: SUBTRACT small from medium GIVING big
           MOVE 10 TO WS-SMALL.
           MOVE 1000 TO WS-MED.
           SUBTRACT WS-SMALL FROM WS-MED GIVING WS-BIG.
           IF WS-BIG = 990
               DISPLAY "NC106A-TEST-2 PASS"
           ELSE
               DISPLAY "NC106A-TEST-2 FAIL"
           END-IF.
      * Test 3: MULTIPLY small by medium GIVING big
           MOVE 99 TO WS-SMALL.
           MOVE 0100 TO WS-MED.
           MULTIPLY WS-SMALL BY WS-MED GIVING WS-BIG.
           IF WS-BIG = 9900
               DISPLAY "NC106A-TEST-3 PASS"
           ELSE
               DISPLAY "NC106A-TEST-3 FAIL"
           END-IF.
      * Test 4: SUBTRACT accumulator form
           MOVE 75 TO WS-SMALL.
           MOVE 0200 TO WS-MED.
           SUBTRACT WS-SMALL FROM WS-MED.
           IF WS-MED = 125
               DISPLAY "NC106A-TEST-4 PASS"
           ELSE
               DISPLAY "NC106A-TEST-4 FAIL"
           END-IF.
      * Test 5: Chained ADD operations
           MOVE 0 TO WS-BIG.
           MOVE 11 TO WS-SMALL.
           ADD WS-SMALL TO WS-BIG.
           ADD WS-SMALL TO WS-BIG.
           ADD WS-SMALL TO WS-BIG.
           ADD WS-SMALL TO WS-BIG.
           IF WS-BIG = 44
               DISPLAY "NC106A-TEST-5 PASS"
           ELSE
               DISPLAY "NC106A-TEST-5 FAIL"
           END-IF.
           STOP RUN.
