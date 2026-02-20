       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM216A.
      *
      * NIST CCVS-style test: COPY WITH REPLACING operand
      * Tests COPY REPLACING to substitute a data name prefix.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-EXPECTED-TEXT       PIC X(20).
       01  WS-EXPECTED-NUM        PIC 9(4).
       COPY SMCPY16
           REPLACING ==WS-PLACEHOLDER==
                  BY ==WS-ACTUAL-NAME==.
       PROCEDURE DIVISION.
      * Test 1: VERIFY REPLACED TEXT FIELD EXISTS AND IS WRITABLE
           MOVE "REPLACEMENT OK" TO WS-ACTUAL-NAME.
           IF WS-ACTUAL-NAME = "REPLACEMENT OK"
               DISPLAY "SM216A-TEST-1 PASS"
           ELSE
               DISPLAY "SM216A-TEST-1 FAIL"
               DISPLAY "  Got " WS-ACTUAL-NAME
           END-IF.
      * Test 2: VERIFY REPLACED NUMERIC FIELD EXISTS AND HAS VALUE
           IF WS-ACTUAL-NAME-NUM = 1234
               DISPLAY "SM216A-TEST-2 PASS"
           ELSE
               DISPLAY "SM216A-TEST-2 FAIL"
               DISPLAY "  Expected 1234, got " WS-ACTUAL-NAME-NUM
           END-IF.
      * Test 3: VERIFY REPLACED FIELDS ARE MODIFIABLE
           MOVE "MODIFIED VALUE" TO WS-ACTUAL-NAME.
           MOVE 5678 TO WS-ACTUAL-NAME-NUM.
           IF WS-ACTUAL-NAME = "MODIFIED VALUE"
               AND WS-ACTUAL-NAME-NUM = 5678
               DISPLAY "SM216A-TEST-3 PASS"
           ELSE
               DISPLAY "SM216A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
