       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM207A.
      *
      * NIST CCVS-style test: COPY REPLACING with pseudo-text
      * Tests that ==old-text== BY ==new-text== pseudo-text
      * delimiters correctly substitute data names in a copybook.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM207A-CPY REPLACING ==OLD-ALPHA== BY ==WS-ALPHA==
                                  ==OLD-NUM== BY ==WS-NUM==
                                  ==OLD-FLAG== BY ==WS-FLAG==.
       PROCEDURE DIVISION.
      * Test 1: Pseudo-text replaced alpha has correct initial value
           IF WS-ALPHA = "PSEUDO-TXT"
               DISPLAY "SM207A-TEST-1 PASS"
           ELSE
               DISPLAY "SM207A-TEST-1 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA "<"
           END-IF.
      * Test 2: Pseudo-text replaced numeric has correct value
           IF WS-NUM = 1234
               DISPLAY "SM207A-TEST-2 PASS"
           ELSE
               DISPLAY "SM207A-TEST-2 FAIL"
               DISPLAY "  NUM=" WS-NUM
           END-IF.
      * Test 3: Pseudo-text replaced flag has correct value
           IF WS-FLAG = "YES"
               DISPLAY "SM207A-TEST-3 PASS"
           ELSE
               DISPLAY "SM207A-TEST-3 FAIL"
               DISPLAY "  FLAG=>" WS-FLAG "<"
           END-IF.
           STOP RUN.
