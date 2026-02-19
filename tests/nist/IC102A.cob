       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC102A.
      *
      * NIST CCVS-style test: CALL with multiple parameters
      * Tests CALL passing multiple parameters BY REFERENCE.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(10) VALUE SPACES.
       01 WS-CODE PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: CALL with two parameters
           CALL "IC102A-SUB" USING WS-NAME WS-CODE.
           IF WS-NAME = "RETURNED  " AND WS-CODE = 99
               DISPLAY "IC102A-TEST-1 PASS"
           ELSE
               DISPLAY "IC102A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-NAME "<"
               DISPLAY "  CODE=" WS-CODE
           END-IF.
      * Test 2: Parameters retain values after CALL returns
           MOVE "BEFORE    " TO WS-NAME.
           MOVE 11 TO WS-CODE.
           CALL "IC102A-SUB" USING WS-NAME WS-CODE.
           IF WS-NAME = "RETURNED  " AND WS-CODE = 99
               DISPLAY "IC102A-TEST-2 PASS"
           ELSE
               DISPLAY "IC102A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
