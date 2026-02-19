       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM212A.
      *
      * NIST CCVS-style test: COPY REPLACING with LEADING
      * Tests that LEADING replacement changes the prefix of
      * every word that starts with the specified text.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM212A-CPY REPLACING LEADING ==PFX== BY ==WS==.
       PROCEDURE DIVISION.
      * Test 1: LEADING replaced alpha field has correct value
           IF WS-NAME = "LEAD-OK   "
               DISPLAY "SM212A-TEST-1 PASS"
           ELSE
               DISPLAY "SM212A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-NAME "<"
           END-IF.
      * Test 2: LEADING replaced numeric field has correct value
           IF WS-CODE = 9999
               DISPLAY "SM212A-TEST-2 PASS"
           ELSE
               DISPLAY "SM212A-TEST-2 FAIL"
               DISPLAY "  CODE=" WS-CODE
           END-IF.
      * Test 3: LEADING replaced flag field has correct value
           IF WS-FLAG = "YES"
               DISPLAY "SM212A-TEST-3 PASS"
           ELSE
               DISPLAY "SM212A-TEST-3 FAIL"
               DISPLAY "  FLAG=>" WS-FLAG "<"
           END-IF.
           STOP RUN.
