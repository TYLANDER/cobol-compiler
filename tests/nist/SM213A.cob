       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM213A.
      *
      * NIST CCVS-style test: COPY REPLACING with TRAILING
      * Tests that TRAILING replacement changes the suffix of
      * every word that ends with the specified text.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM213A-CPY REPLACING TRAILING ==SFX== BY ==WS==.
       PROCEDURE DIVISION.
      * Test 1: TRAILING replaced alpha field has correct value
           IF NAME-WS = "TRAIL-OK  "
               DISPLAY "SM213A-TEST-1 PASS"
           ELSE
               DISPLAY "SM213A-TEST-1 FAIL"
               DISPLAY "  NAME=>" NAME-WS "<"
           END-IF.
      * Test 2: TRAILING replaced numeric field has correct value
           IF CODE-WS = 1111
               DISPLAY "SM213A-TEST-2 PASS"
           ELSE
               DISPLAY "SM213A-TEST-2 FAIL"
               DISPLAY "  CODE=" CODE-WS
           END-IF.
      * Test 3: TRAILING replaced flag field has correct value
           IF FLAG-WS = "OK "
               DISPLAY "SM213A-TEST-3 PASS"
           ELSE
               DISPLAY "SM213A-TEST-3 FAIL"
               DISPLAY "  FLAG=>" FLAG-WS "<"
           END-IF.
           STOP RUN.
