       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC216A.
      *
      * NIST CCVS-style test: CALL with group item parameter.
      * Tests passing a group item (record with sub-fields) to
      * a subprogram and verifying the sub can access and modify
      * individual fields within the group.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RECORD.
           05 WS-REC-NAME  PIC X(10) VALUE SPACES.
           05 WS-REC-CODE  PIC X(4) VALUE SPACES.
           05 WS-REC-AMT   PIC 9(6) VALUE 0.
       01 WS-FLAG   PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Sub populates group item fields
           MOVE SPACES TO WS-REC-NAME.
           MOVE SPACES TO WS-REC-CODE.
           MOVE 0 TO WS-REC-AMT.
           MOVE 0 TO WS-FLAG.
           CALL "IC216A-SUB" USING WS-RECORD WS-FLAG.
           IF WS-REC-NAME = "GROUPTEST "
               DISPLAY "IC216A-TEST-1 PASS"
           ELSE
               DISPLAY "IC216A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-REC-NAME "<"
           END-IF.
      * Test 2: Numeric field within group set correctly
           IF WS-REC-AMT = 5000
               DISPLAY "IC216A-TEST-2 PASS"
           ELSE
               DISPLAY "IC216A-TEST-2 FAIL"
               DISPLAY "  AMT=" WS-REC-AMT
           END-IF.
      * Test 3: Code field and flag set correctly
           IF WS-REC-CODE = "GRP1" AND WS-FLAG = 1
               DISPLAY "IC216A-TEST-3 PASS"
           ELSE
               DISPLAY "IC216A-TEST-3 FAIL"
               DISPLAY "  CODE=>" WS-REC-CODE "<"
               DISPLAY "  FLAG=" WS-FLAG
           END-IF.
           STOP RUN.
