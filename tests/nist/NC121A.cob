       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC121A.
      *
      * NIST CCVS-style test: Mixed subscript operations
      * Tests MOVE from subscripted item, SUBTRACT with subscripted
      * operand, and MULTIPLY with subscripted GIVING target.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
           05 WS-ITEM PIC 9(4) OCCURS 5 TIMES.
       01 WS-DEST      PIC 9(4) VALUE ZEROS.
       01 WS-SUM       PIC 9(8) VALUE ZEROS.
       01 WS-I         PIC 9(2) VALUE ZEROS.
       01 WS-TEMP      PIC 9(8) VALUE ZEROS.
       01 WS-RESULT-TBL.
           05 WS-RES PIC 9(8) OCCURS 5 TIMES.
       PROCEDURE DIVISION.
      * Initialize the table: 10, 20, 30, 40, 50
           MOVE 10 TO WS-ITEM(1).
           MOVE 20 TO WS-ITEM(2).
           MOVE 30 TO WS-ITEM(3).
           MOVE 40 TO WS-ITEM(4).
           MOVE 50 TO WS-ITEM(5).
      * Test 1: MOVE subscripted-item to non-subscripted in a loop
      *   Copy each table element and sum them: 10+20+30+40+50=150
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 5
               MOVE WS-ITEM(WS-I) TO WS-DEST
               ADD WS-DEST TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 150
               DISPLAY "NC121A-TEST-1 PASS"
           ELSE
               DISPLAY "NC121A-TEST-1 FAIL"
               DISPLAY "  Expected 150, got " WS-SUM
           END-IF.
      * Test 2: SUBTRACT with subscripted operand
      *   Start with 100, subtract WS-ITEM(3) which is 30 => 70
           MOVE 100 TO WS-DEST.
           SUBTRACT WS-ITEM(3) FROM WS-DEST.
           IF WS-DEST = 70
               DISPLAY "NC121A-TEST-2 PASS"
           ELSE
               DISPLAY "NC121A-TEST-2 FAIL"
               DISPLAY "  Expected 70, got " WS-DEST
           END-IF.
      * Test 3: MULTIPLY with subscripted operand GIVING target
      *   WS-ITEM(2)=20, multiply by 5, giving WS-TEMP => 100
           MULTIPLY WS-ITEM(2) BY 5 GIVING WS-TEMP.
           IF WS-TEMP = 100
               DISPLAY "NC121A-TEST-3 PASS"
           ELSE
               DISPLAY "NC121A-TEST-3 FAIL"
               DISPLAY "  Expected 100, got " WS-TEMP
           END-IF.
           STOP RUN.
