       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-PERFORM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNT PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
           PERFORM ADD-ONE 5 TIMES.
           IF WS-COUNT = 5
               DISPLAY "PASS"
           ELSE
               DISPLAY "FAIL: " WS-COUNT
           END-IF
           STOP RUN.
       ADD-ONE.
           ADD 1 TO WS-COUNT.
