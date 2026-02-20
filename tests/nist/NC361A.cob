       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC361A.
      *
      * NIST CCVS-style test: Table access with OCCURS
      * Tests simple table definition with single-item entries,
      * subscript access, PERFORM VARYING iteration.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
           05 WS-ITEM PIC 9(3) OCCURS 10 TIMES.
       01 WS-SUB        PIC 99 VALUE 0.
       01 WS-RESULT     PIC 9(3) VALUE 0.
       01 WS-SUM        PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Initialize table
           PERFORM VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > 10
               MOVE WS-SUB TO WS-ITEM(WS-SUB)
           END-PERFORM.
      * Test 1: Read element 5 via literal subscript
           MOVE WS-ITEM(5) TO WS-RESULT.
           IF WS-RESULT = 5
               DISPLAY "NC361A-TEST-1 PASS"
           ELSE
               DISPLAY "NC361A-TEST-1 FAIL"
               DISPLAY "  Expected 005, got " WS-RESULT
           END-IF.
      * Test 2: Read element via variable subscript
           MOVE 8 TO WS-SUB.
           MOVE WS-ITEM(WS-SUB) TO WS-RESULT.
           IF WS-RESULT = 8
               DISPLAY "NC361A-TEST-2 PASS"
           ELSE
               DISPLAY "NC361A-TEST-2 FAIL"
               DISPLAY "  Expected 008, got " WS-RESULT
           END-IF.
      * Test 3: Sum all elements (1+2+...+10 = 55)
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > 10
               ADD WS-ITEM(WS-SUB) TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 55
               DISPLAY "NC361A-TEST-3 PASS"
           ELSE
               DISPLAY "NC361A-TEST-3 FAIL"
               DISPLAY "  Expected 0055, got " WS-SUM
           END-IF.
           STOP RUN.
