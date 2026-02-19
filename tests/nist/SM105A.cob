       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM105A.
      *
      * NIST CCVS-style test: COPY REPLACING LEADING
      * Tests COPY with LEADING prefix replacement.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM-LEADING REPLACING LEADING ==:PFX:== BY ==WS==.
       PROCEDURE DIVISION.
      * Test 1: LEADING replacement changes prefix
           IF WS-FIRST = "AAAAA" AND WS-SECOND = "BBBBB"
               DISPLAY "SM105A-TEST-1 PASS"
           ELSE
               DISPLAY "SM105A-TEST-1 FAIL"
               DISPLAY "  FIRST=>" WS-FIRST "<"
               DISPLAY "  SECOND=>" WS-SECOND "<"
           END-IF.
      * Test 2: Modify and verify
           MOVE "CCCCC" TO WS-FIRST.
           IF WS-FIRST = "CCCCC"
               DISPLAY "SM105A-TEST-2 PASS"
           ELSE
               DISPLAY "SM105A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
