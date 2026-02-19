       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-FIGCONST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(10) VALUE "HELLO".
       01 WS-NUM  PIC 9(4) VALUE 1234.
       01 WS-PASS PIC 9 VALUE 0.
       PROCEDURE DIVISION.
           MOVE SPACES TO WS-NAME.
           IF WS-NAME = SPACES
               ADD 1 TO WS-PASS
           END-IF
           MOVE ZEROS TO WS-NUM.
           IF WS-NUM = ZEROS
               ADD 1 TO WS-PASS
           END-IF
           IF WS-PASS = 2
               DISPLAY "PASS"
           ELSE
               DISPLAY "FAIL: " WS-PASS
           END-IF
           STOP RUN.
