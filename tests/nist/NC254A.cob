       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC254A.
      *
      * NIST CCVS-style test: SET index-name
      * Tests SET IDX TO 1, SET IDX UP BY 1, SET IDX DOWN BY 1
      * with a table defined using OCCURS and INDEXED BY.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
           05 WS-ENTRY OCCURS 5 TIMES
               INDEXED BY WS-IDX
               PIC 9(4).
       01 WS-RESULT    PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC254A-CONTROL.
           PERFORM NC254A-TEST-1.
           PERFORM NC254A-TEST-2.
           PERFORM NC254A-TEST-3.
           STOP RUN.
       NC254A-TEST-1.
      * SET index TO 1, store and retrieve value
      *   Load table, SET IDX TO 3, read 3rd element
           MOVE 10 TO WS-ENTRY(1).
           MOVE 20 TO WS-ENTRY(2).
           MOVE 30 TO WS-ENTRY(3).
           MOVE 40 TO WS-ENTRY(4).
           MOVE 50 TO WS-ENTRY(5).
           SET WS-IDX TO 3.
           MOVE WS-ENTRY(WS-IDX) TO WS-RESULT.
           IF WS-RESULT = 30
               DISPLAY "NC254A-TEST-1 PASS"
           ELSE
               DISPLAY "NC254A-TEST-1 FAIL"
               DISPLAY "  Expected 30, got " WS-RESULT
           END-IF.
       NC254A-TEST-2.
      * SET IDX UP BY 1: from index 3 to index 4
           SET WS-IDX TO 3.
           SET WS-IDX UP BY 1.
           MOVE WS-ENTRY(WS-IDX) TO WS-RESULT.
           IF WS-RESULT = 40
               DISPLAY "NC254A-TEST-2 PASS"
           ELSE
               DISPLAY "NC254A-TEST-2 FAIL"
               DISPLAY "  Expected 40, got " WS-RESULT
           END-IF.
       NC254A-TEST-3.
      * SET IDX DOWN BY 1: from index 5 to index 4
           SET WS-IDX TO 5.
           SET WS-IDX DOWN BY 1.
           MOVE WS-ENTRY(WS-IDX) TO WS-RESULT.
           IF WS-RESULT = 40
               DISPLAY "NC254A-TEST-3 PASS"
           ELSE
               DISPLAY "NC254A-TEST-3 FAIL"
               DISPLAY "  Expected 40, got " WS-RESULT
           END-IF.
