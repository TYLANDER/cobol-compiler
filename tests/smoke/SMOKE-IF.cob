       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-IF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I PIC 9(2) VALUE 0.
       01 WS-SUM PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 10
               ADD WS-I TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 55
               DISPLAY "PASS"
           ELSE
               DISPLAY "FAIL"
           END-IF.
           STOP RUN.
