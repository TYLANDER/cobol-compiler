       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM104A.
      *
      * NIST CCVS-style test: COPY in PROCEDURE DIVISION
      * Tests COPY of procedure statements.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SM-RESULT PIC X(7) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: COPY of procedure statements
           COPY SM-PROC.
           IF SM-RESULT = "PROC-OK"
               DISPLAY "SM104A-TEST-1 PASS"
           ELSE
               DISPLAY "SM104A-TEST-1 FAIL"
               DISPLAY "  RESULT=>" SM-RESULT "<"
           END-IF.
           STOP RUN.
