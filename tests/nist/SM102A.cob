       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM102A.
      *
      * NIST CCVS-style test: COPY REPLACING
      * Tests COPY with pseudo-text REPLACING.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM-REPLACE REPLACING ==:TAG:== BY ==WS==.
       PROCEDURE DIVISION.
      * Test 1: COPY REPLACING changes field names
           MOVE "HELLO     " TO WS-NAME.
           MOVE 42 TO WS-COUNT.
           IF WS-NAME = "HELLO     " AND WS-COUNT = 42
               DISPLAY "SM102A-TEST-1 PASS"
           ELSE
               DISPLAY "SM102A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-NAME "<"
               DISPLAY "  COUNT=" WS-COUNT
           END-IF.
      * Test 2: Multiple uses of same copybook with different replacements
      *   (This test just validates that the first copy works correctly
      *    for arithmetic operations on the replaced fields)
           ADD 8 TO WS-COUNT.
           IF WS-COUNT = 50
               DISPLAY "SM102A-TEST-2 PASS"
           ELSE
               DISPLAY "SM102A-TEST-2 FAIL"
               DISPLAY "  COUNT=" WS-COUNT
           END-IF.
           STOP RUN.
