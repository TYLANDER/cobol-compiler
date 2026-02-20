       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM217A.
      *
      * NIST CCVS-style test: COPY IN PROCEDURE DIVISION
      * Tests that COPY in procedure division inserts executable
      * statements.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESULT              PIC X(10) VALUE SPACES.
       01  WS-COUNTER             PIC 9(4)  VALUE 5.
       01  WS-STATUS              PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Execute statements from copied copybook
           COPY SMCPY17.
      * Test 1: VERIFY MOVE STATEMENT FROM COPY EXECUTED
           IF WS-RESULT = "COPY-PROC"
               DISPLAY "SM217A-TEST-1 PASS"
           ELSE
               DISPLAY "SM217A-TEST-1 FAIL"
               DISPLAY "  Expected COPY-PROC, got " WS-RESULT
           END-IF.
      * Test 2: VERIFY ADD STATEMENT FROM COPY EXECUTED
           IF WS-COUNTER = 15
               DISPLAY "SM217A-TEST-2 PASS"
           ELSE
               DISPLAY "SM217A-TEST-2 FAIL"
               DISPLAY "  Expected 15, got " WS-COUNTER
           END-IF.
      * Test 3: VERIFY STATUS FIELD SET BY COPY
           IF WS-STATUS = "EXECUTED"
               DISPLAY "SM217A-TEST-3 PASS"
           ELSE
               DISPLAY "SM217A-TEST-3 FAIL"
               DISPLAY "  Expected EXECUTED, got " WS-STATUS
           END-IF.
           STOP RUN.
