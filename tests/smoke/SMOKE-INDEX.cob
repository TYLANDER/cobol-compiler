       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-INDEX.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
           05 WS-ITEM PIC 9(3) OCCURS 10 TIMES
               INDEXED BY WS-IDX.
       01 WS-SUB PIC 99 VALUE 0.
       01 WS-RESULT PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
           PERFORM VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > 10
               MOVE WS-SUB TO WS-ITEM(WS-SUB)
           END-PERFORM.
           SET WS-IDX TO 5.
           MOVE WS-ITEM(WS-IDX) TO WS-RESULT.
           IF WS-RESULT = 5
               DISPLAY "SET-TO PASS"
           ELSE
               DISPLAY "SET-TO FAIL GOT " WS-RESULT
           END-IF.
           SET WS-IDX TO 3.
           SET WS-IDX UP BY 4.
           MOVE WS-ITEM(WS-IDX) TO WS-RESULT.
           IF WS-RESULT = 7
               DISPLAY "SET-UP PASS"
           ELSE
               DISPLAY "SET-UP FAIL GOT " WS-RESULT
           END-IF.
           STOP RUN.
