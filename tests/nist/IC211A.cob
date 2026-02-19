       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC211A.
      *
      * NIST CCVS-style test: CALL with BY REFERENCE parameter
      * modification. The subprogram modifies parameters passed
      * BY REFERENCE, and the caller verifies changes are visible.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM    PIC 9(4) VALUE 0.
       01 WS-ALPHA  PIC X(10) VALUE SPACES.
       01 WS-FLAG   PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Numeric parameter modified by sub via BY REFERENCE
           MOVE 100 TO WS-NUM.
           CALL "IC211A-SUB" USING BY REFERENCE WS-NUM
                                   BY REFERENCE WS-ALPHA
                                   BY REFERENCE WS-FLAG.
           IF WS-NUM = 999
               DISPLAY "IC211A-TEST-1 PASS"
           ELSE
               DISPLAY "IC211A-TEST-1 FAIL"
               DISPLAY "  NUM=" WS-NUM
           END-IF.
      * Test 2: Alphanumeric parameter modified by sub
           IF WS-ALPHA = "REFCHANGE "
               DISPLAY "IC211A-TEST-2 PASS"
           ELSE
               DISPLAY "IC211A-TEST-2 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA "<"
           END-IF.
      * Test 3: Call again with new values, verify second modification
           MOVE 200 TO WS-NUM.
           MOVE SPACES TO WS-ALPHA.
           MOVE 0 TO WS-FLAG.
           CALL "IC211A-SUB" USING BY REFERENCE WS-NUM
                                   BY REFERENCE WS-ALPHA
                                   BY REFERENCE WS-FLAG.
           IF WS-NUM = 999 AND WS-FLAG = 2
               DISPLAY "IC211A-TEST-3 PASS"
           ELSE
               DISPLAY "IC211A-TEST-3 FAIL"
               DISPLAY "  NUM=" WS-NUM
               DISPLAY "  FLAG=" WS-FLAG
           END-IF.
           STOP RUN.
