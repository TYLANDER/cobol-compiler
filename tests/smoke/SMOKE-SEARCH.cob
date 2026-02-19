       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-SEARCH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
          05 WS-ENTRY OCCURS 5 TIMES
             INDEXED BY WS-IDX.
             10 WS-VALUE PIC 9(2).
       01 WS-SEARCH-VAL PIC 9(2) VALUE 30.
       PROCEDURE DIVISION.
           MOVE 10 TO WS-VALUE(1).
           MOVE 20 TO WS-VALUE(2).
           MOVE 30 TO WS-VALUE(3).
           MOVE 40 TO WS-VALUE(4).
           MOVE 50 TO WS-VALUE(5).
           SET WS-IDX TO 1.
           SEARCH WS-ENTRY
               AT END DISPLAY "FAIL: NOT FOUND"
               WHEN WS-VALUE(WS-IDX) = WS-SEARCH-VAL
                   DISPLAY "PASS"
           END-SEARCH
           STOP RUN.
