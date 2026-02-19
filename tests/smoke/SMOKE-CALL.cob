       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-CALL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
           CALL "SMOKE-SUB" USING WS-RESULT.
           IF WS-RESULT = 42
               DISPLAY "PASS"
           ELSE
               DISPLAY "FAIL"
           END-IF.
           STOP RUN.
