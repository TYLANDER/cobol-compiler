       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM205A.
      *
      * NIST CCVS-style test: COPY REPLACING with multiple pairs
      * Tests COPY with many REPLACING pairs including both
      * data-name and literal substitutions.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM205A-CPY REPLACING ==:PFX:== BY ==WS==
                                  ==:LIT-NAME:== BY =="MULTI-RPL "==
                                  ==:LIT-CODE:== BY ==8888==
                                  ==:LIT-FLAG:== BY =="YES"==.
       PROCEDURE DIVISION.
      * Test 1: All name replacements applied correctly
           IF WS-NAME = "MULTI-RPL "
               DISPLAY "SM205A-TEST-1 PASS"
           ELSE
               DISPLAY "SM205A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-NAME "<"
           END-IF.
      * Test 2: Numeric literal replacement
           IF WS-CODE = 8888
               DISPLAY "SM205A-TEST-2 PASS"
           ELSE
               DISPLAY "SM205A-TEST-2 FAIL"
               DISPLAY "  CODE=" WS-CODE
           END-IF.
      * Test 3: Flag literal replacement and modification
           IF WS-FLAG = "YES"
               DISPLAY "SM205A-TEST-3 PASS"
           ELSE
               DISPLAY "SM205A-TEST-3 FAIL"
               DISPLAY "  FLAG=>" WS-FLAG "<"
           END-IF.
           STOP RUN.
