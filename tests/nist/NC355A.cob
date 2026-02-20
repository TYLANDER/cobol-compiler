       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC355A.
      *
      * NIST CCVS-style test: OCCURS DEPENDING ON
      * (variable-length tables). Define a table with
      * OCCURS 1 TO 10 DEPENDING ON, fill and read entries.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE-SIZE     PIC 9(2) VALUE 0.
       01 WS-TABLE-GROUP.
          05 WS-ENTRY       PIC 9(3)
                             OCCURS 1 TO 10 TIMES
                             DEPENDING ON WS-TABLE-SIZE.
       01 WS-INDEX          PIC 9(2) VALUE 0.
       01 WS-SUM            PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Fill table with 5 entries and read back
      *   Store values 10, 20, 30, 40, 50 and verify
      *   entry 3 is 30.
           MOVE 5 TO WS-TABLE-SIZE.
           MOVE 10 TO WS-ENTRY(1).
           MOVE 20 TO WS-ENTRY(2).
           MOVE 30 TO WS-ENTRY(3).
           MOVE 40 TO WS-ENTRY(4).
           MOVE 50 TO WS-ENTRY(5).
           IF WS-ENTRY(3) = 30
               DISPLAY "NC355A-TEST-1 PASS"
           ELSE
               DISPLAY "NC355A-TEST-1 FAIL"
               DISPLAY "  Expected entry(3)=30, got "
                       WS-ENTRY(3)
           END-IF.
      * Test 2: Sum all entries in variable-length table
      *   Sum of 10+20+30+40+50 = 150.
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-TABLE-SIZE
               ADD WS-ENTRY(WS-INDEX) TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 150
               DISPLAY "NC355A-TEST-2 PASS"
           ELSE
               DISPLAY "NC355A-TEST-2 FAIL"
               DISPLAY "  Expected sum=150, got " WS-SUM
           END-IF.
      * Test 3: Resize table and verify new size
      *   Reduce to 3 entries and sum: 10+20+30 = 60.
           MOVE 3 TO WS-TABLE-SIZE.
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-TABLE-SIZE
               ADD WS-ENTRY(WS-INDEX) TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 60
               DISPLAY "NC355A-TEST-3 PASS"
           ELSE
               DISPLAY "NC355A-TEST-3 FAIL"
               DISPLAY "  Expected sum=60, got " WS-SUM
           END-IF.
           STOP RUN.
