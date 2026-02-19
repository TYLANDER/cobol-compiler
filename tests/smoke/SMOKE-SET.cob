       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-SET.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STATUS PIC X(3) VALUE "NO ".
          88 WS-ACTIVE VALUE "YES".
          88 WS-INACTIVE VALUE "NO ".
       01 WS-IDX PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
           SET WS-ACTIVE TO TRUE
           IF WS-STATUS = "YES"
               DISPLAY "TEST-1 PASS"
           ELSE
               DISPLAY "TEST-1 FAIL"
               DISPLAY WS-STATUS
           END-IF
           SET WS-INACTIVE TO TRUE
           IF WS-STATUS = "NO "
               DISPLAY "TEST-2 PASS"
           ELSE
               DISPLAY "TEST-2 FAIL"
               DISPLAY WS-STATUS
           END-IF
           STOP RUN.
