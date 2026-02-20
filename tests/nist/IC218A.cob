       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC218A.
      *
      * NIST CCVS-style test: CALL WITH MULTIPLE BY REFERENCE
      * Tests that the subprogram can modify multiple parameters
      * passed BY REFERENCE.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FIELD1             PIC X(10) VALUE SPACES.
       01  WS-FIELD2             PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Sub modifies first field
           MOVE "ORIGINAL" TO WS-FIELD1.
           MOVE "ORIGINAL" TO WS-FIELD2.
           CALL "IC218A-SUB" USING WS-FIELD1 WS-FIELD2.
           IF WS-FIELD1 = "MODIFIED"
               DISPLAY "IC218A-TEST-1 PASS"
           ELSE
               DISPLAY "IC218A-TEST-1 FAIL"
               DISPLAY "  Expected MODIFIED, got " WS-FIELD1
           END-IF.
      * Test 2: Sub modifies second field
           IF WS-FIELD2 = "CHANGED"
               DISPLAY "IC218A-TEST-2 PASS"
           ELSE
               DISPLAY "IC218A-TEST-2 FAIL"
               DISPLAY "  Expected CHANGED, got " WS-FIELD2
           END-IF.
      * Test 3: Call again with different values
           MOVE "BEFORE" TO WS-FIELD1.
           MOVE "BEFORE" TO WS-FIELD2.
           CALL "IC218A-SUB" USING WS-FIELD1 WS-FIELD2.
           IF WS-FIELD1 = "MODIFIED"
               AND WS-FIELD2 = "CHANGED"
               DISPLAY "IC218A-TEST-3 PASS"
           ELSE
               DISPLAY "IC218A-TEST-3 FAIL"
               DISPLAY "  FIELD1=" WS-FIELD1
               DISPLAY "  FIELD2=" WS-FIELD2
           END-IF.
           STOP RUN.
