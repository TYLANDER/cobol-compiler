       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-INIT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-REC.
          05 WS-NAME PIC X(10) VALUE "DIRTY".
          05 WS-NUM  PIC 9(4)  VALUE 9999.
       PROCEDURE DIVISION.
           INITIALIZE WS-REC.
           IF WS-NAME = SPACES AND WS-NUM = ZEROS
               DISPLAY "PASS"
           ELSE
               DISPLAY "FAIL: " WS-NAME " " WS-NUM
           END-IF
           STOP RUN.
