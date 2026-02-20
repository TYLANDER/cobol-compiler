       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM219A.
      *
      * NIST CCVS-style test: COPY REPLACING LEADING
      * Tests COPY with REPLACING LEADING to substitute a prefix
      * in data names.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SMCPY19
           REPLACING LEADING ==PRE== BY ==ACT==.
       PROCEDURE DIVISION.
      * Test 1: VERIFY WS-ACT-FIELD1 EXISTS WITH CORRECT VALUE
           IF WS-ACT-FIELD1 = "LEADING-OK"
               DISPLAY "SM219A-TEST-1 PASS"
           ELSE
               DISPLAY "SM219A-TEST-1 FAIL"
               DISPLAY "  Got " WS-ACT-FIELD1
           END-IF.
      * Test 2: VERIFY WS-ACT-FIELD2 EXISTS WITH CORRECT VALUE
           IF WS-ACT-FIELD2 = 7777
               DISPLAY "SM219A-TEST-2 PASS"
           ELSE
               DISPLAY "SM219A-TEST-2 FAIL"
               DISPLAY "  Got " WS-ACT-FIELD2
           END-IF.
      * Test 3: VERIFY WS-ACT-COUNTER EXISTS AND IS MODIFIABLE
           ADD 50 TO WS-ACT-COUNTER.
           IF WS-ACT-COUNTER = 150
               DISPLAY "SM219A-TEST-3 PASS"
           ELSE
               DISPLAY "SM219A-TEST-3 FAIL"
               DISPLAY "  Expected 150, got " WS-ACT-COUNTER
           END-IF.
           STOP RUN.
