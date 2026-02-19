       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM202A.
      *
      * NIST CCVS-style test: COPY REPLACING LEADING/TRAILING
      * Tests COPY with LEADING and TRAILING replacement.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM202A-CPY REPLACING LEADING ==XX== BY ==WS==
                                 TRAILING ==YY== BY ==A1==.
       PROCEDURE DIVISION.
      * Test 1: LEADING replacement changes prefix XX to WS
           IF WS-FIRST = "LEAD-OK " AND WS-SECOND = "LEAD-TWO"
               DISPLAY "SM202A-TEST-1 PASS"
           ELSE
               DISPLAY "SM202A-TEST-1 FAIL"
               DISPLAY "  FIRST=>" WS-FIRST "<"
               DISPLAY "  SECOND=>" WS-SECOND "<"
           END-IF.
      * Test 2: TRAILING replacement changes suffix YY to A1
           IF ITEM-A1 = "TRAIL-OK" AND COUNT-A1 = 7777
               DISPLAY "SM202A-TEST-2 PASS"
           ELSE
               DISPLAY "SM202A-TEST-2 FAIL"
               DISPLAY "  ITEM=>" ITEM-A1 "<"
               DISPLAY "  COUNT=" COUNT-A1
           END-IF.
      * Test 3: Modify LEADING-replaced field
           MOVE "CHANGED " TO WS-FIRST.
           IF WS-FIRST = "CHANGED "
               DISPLAY "SM202A-TEST-3 PASS"
           ELSE
               DISPLAY "SM202A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
