       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC103A.
      *
      * NIST CCVS-style test: GOBACK from subprogram
      * Tests that GOBACK returns control to caller.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STATUS PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: GOBACK returns control to caller
           CALL "IC103A-SUB" USING WS-STATUS.
           IF WS-STATUS = 77
               DISPLAY "IC103A-TEST-1 PASS"
           ELSE
               DISPLAY "IC103A-TEST-1 FAIL"
               DISPLAY "  STATUS=" WS-STATUS
           END-IF.
      * Test 2: Execution continues after CALL returns via GOBACK
           DISPLAY "IC103A-TEST-2 PASS".
           STOP RUN.
