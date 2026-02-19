       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-GOBACK.
      * Test GOBACK in a no-USING subprogram returns to caller.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESULT      PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
           DISPLAY "MAIN-START".
           CALL "GOBACKSUB".
           DISPLAY "MAIN-END".
           STOP RUN.
