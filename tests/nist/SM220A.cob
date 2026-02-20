       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM220A.
      *
      * NIST CCVS-style test: Multiple COPY in same program
      * Tests two COPY statements - one in DATA DIVISION and
      * one in PROCEDURE DIVISION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROC-RESULT        PIC X(15) VALUE SPACES.
       COPY SMCPY20D.
       PROCEDURE DIVISION.
      * Execute statements from procedure division copybook
           COPY SMCPY20P.
      * Test 1: VERIFY DATA DIVISION COPY - TEXT FIELD
           IF WS-DATA-FIELD1 = "DATA-COPY-OK"
               DISPLAY "SM220A-TEST-1 PASS"
           ELSE
               DISPLAY "SM220A-TEST-1 FAIL"
               DISPLAY "  Got " WS-DATA-FIELD1
           END-IF.
      * Test 2: VERIFY PROCEDURE DIVISION COPY EXECUTED
           IF WS-PROC-RESULT = "PROC-COPY-OK"
               DISPLAY "SM220A-TEST-2 PASS"
           ELSE
               DISPLAY "SM220A-TEST-2 FAIL"
               DISPLAY "  Got " WS-PROC-RESULT
           END-IF.
      * Test 3: VERIFY BOTH COPIES INTERACTED
           IF WS-DATA-FIELD2 = 10005
               DISPLAY "SM220A-TEST-3 PASS"
           ELSE
               DISPLAY "SM220A-TEST-3 FAIL"
               DISPLAY "  Expected 10005, got " WS-DATA-FIELD2
           END-IF.
           STOP RUN.
