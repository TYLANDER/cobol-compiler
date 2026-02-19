       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-EVALUATE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GRADE PIC 9 VALUE 3.
       01 WS-RESULT PIC X(4) VALUE SPACES.
       PROCEDURE DIVISION.
           EVALUATE WS-GRADE
               WHEN 1
                   MOVE "FAIL" TO WS-RESULT
               WHEN 2
                   MOVE "FAIL" TO WS-RESULT
               WHEN 3
                   MOVE "PASS" TO WS-RESULT
               WHEN OTHER
                   MOVE "FAIL" TO WS-RESULT
           END-EVALUATE
           DISPLAY WS-RESULT
           STOP RUN.
