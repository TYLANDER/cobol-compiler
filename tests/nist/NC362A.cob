       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC362A.
      *
      * NIST CCVS-style test: Table SEARCH with single-item OCCURS
      * Tests table lookup using PERFORM VARYING loop.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
           05 WS-ITEM PIC 9(3) OCCURS 5 TIMES.
       01 WS-SUB        PIC 99 VALUE 0.
       01 WS-FOUND      PIC 9(3) VALUE 0.
       01 WS-FLAG       PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Load specific values into table
           MOVE 100 TO WS-ITEM(1).
           MOVE 250 TO WS-ITEM(2).
           MOVE 375 TO WS-ITEM(3).
           MOVE 400 TO WS-ITEM(4).
           MOVE 500 TO WS-ITEM(5).
      * Test 1: Search for value 375 (element 3)
           MOVE 0 TO WS-FLAG.
           MOVE 0 TO WS-FOUND.
           PERFORM VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > 5
               IF WS-ITEM(WS-SUB) = 375
                   MOVE WS-ITEM(WS-SUB) TO WS-FOUND
                   MOVE 1 TO WS-FLAG
               END-IF
           END-PERFORM.
           IF WS-FLAG = 1 AND WS-FOUND = 375
               DISPLAY "NC362A-TEST-1 PASS"
           ELSE
               DISPLAY "NC362A-TEST-1 FAIL"
               DISPLAY "  Expected 375, got " WS-FOUND
           END-IF.
      * Test 2: Search for non-existent value 999
           MOVE 0 TO WS-FLAG.
           PERFORM VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > 5
               IF WS-ITEM(WS-SUB) = 999
                   MOVE 1 TO WS-FLAG
               END-IF
           END-PERFORM.
           IF WS-FLAG = 0
               DISPLAY "NC362A-TEST-2 PASS"
           ELSE
               DISPLAY "NC362A-TEST-2 FAIL"
               DISPLAY "  Found 999 when should not exist"
           END-IF.
      * Test 3: Search for first element (value 100)
           MOVE 0 TO WS-FLAG.
           MOVE 0 TO WS-FOUND.
           PERFORM VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > 5
               IF WS-ITEM(WS-SUB) = 100
                   MOVE WS-ITEM(WS-SUB) TO WS-FOUND
                   MOVE 1 TO WS-FLAG
               END-IF
           END-PERFORM.
           IF WS-FLAG = 1 AND WS-FOUND = 100
               DISPLAY "NC362A-TEST-3 PASS"
           ELSE
               DISPLAY "NC362A-TEST-3 FAIL"
               DISPLAY "  Expected 100, got " WS-FOUND
           END-IF.
           STOP RUN.
