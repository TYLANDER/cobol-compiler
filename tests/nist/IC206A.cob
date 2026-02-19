       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC206A.
      *
      * NIST CCVS-style test: CALL with literal program name
      * Tests using a literal string for the program name in
      * the CALL statement rather than an identifier.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FLAG   PIC 9(4) VALUE 0.
       01 WS-TEXT   PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: CALL literal sets numeric flag
           CALL "IC206A-SUB" USING WS-FLAG WS-TEXT.
           IF WS-FLAG = 1
               DISPLAY "IC206A-TEST-1 PASS"
           ELSE
               DISPLAY "IC206A-TEST-1 FAIL"
               DISPLAY "  FLAG=" WS-FLAG
           END-IF.
      * Test 2: CALL literal sets alphanumeric text
           IF WS-TEXT = "CALLED    "
               DISPLAY "IC206A-TEST-2 PASS"
           ELSE
               DISPLAY "IC206A-TEST-2 FAIL"
               DISPLAY "  TEXT=>" WS-TEXT "<"
           END-IF.
      * Test 3: Second CALL with literal name works consistently
           MOVE 0 TO WS-FLAG.
           MOVE SPACES TO WS-TEXT.
           CALL "IC206A-SUB" USING WS-FLAG WS-TEXT.
           IF WS-FLAG = 1 AND WS-TEXT = "CALLED    "
               DISPLAY "IC206A-TEST-3 PASS"
           ELSE
               DISPLAY "IC206A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
