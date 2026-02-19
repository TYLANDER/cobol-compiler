       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC168A.
      *
      * NIST CCVS-style test: OCCURS DEPENDING ON
      * Tests variable-length tables using OCCURS DEPENDING ON
      * to control the effective number of table entries.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-LIMIT        PIC 9(2)  VALUE 3.
       01 WS-TABLE.
           05 WS-ITEM OCCURS 1 TO 10 TIMES
               DEPENDING ON WS-LIMIT
               PIC 9(3).
       01 WS-SUM           PIC 9(6) VALUE ZEROS.
       01 WS-I             PIC 9(2) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Set LIMIT=3, fill 3 entries, sum them
      *   ITEM(1)=10, ITEM(2)=20, ITEM(3)=30, SUM=60
           MOVE 3 TO WS-LIMIT.
           MOVE 10 TO WS-ITEM(1).
           MOVE 20 TO WS-ITEM(2).
           MOVE 30 TO WS-ITEM(3).
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-LIMIT
               ADD WS-ITEM(WS-I) TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 60
               DISPLAY "NC168A-TEST-1 PASS"
           ELSE
               DISPLAY "NC168A-TEST-1 FAIL"
               DISPLAY "  Expected SUM=60, got " WS-SUM
           END-IF.
      * Test 2: Change LIMIT to 5, add two more entries
      *   ITEM(4)=40, ITEM(5)=50
      *   Sum all 5: 10+20+30+40+50=150
           MOVE 5 TO WS-LIMIT.
           MOVE 40 TO WS-ITEM(4).
           MOVE 50 TO WS-ITEM(5).
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-LIMIT
               ADD WS-ITEM(WS-I) TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 150
               DISPLAY "NC168A-TEST-2 PASS"
           ELSE
               DISPLAY "NC168A-TEST-2 FAIL"
               DISPLAY "  Expected SUM=150, got " WS-SUM
           END-IF.
           STOP RUN.
