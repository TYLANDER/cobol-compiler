       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM209A.
      *
      * NIST CCVS-style test: COPY REPLACING multiple pairs
      * Tests COPY with four REPLACING pairs applied simultaneously,
      * substituting both prefix names and literal values.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM209A-CPY REPLACING ==:P:== BY ==WS==
                                  ==:V1:== BY =="MULTI   "==
                                  ==:V2:== BY ==5678==
                                  ==:V3:== BY =="ABCDE"==
                                  ==:V4:== BY ==999999==.
       PROCEDURE DIVISION.
      * Test 1: First replacement pair - alpha field
           IF WS-FIRST = "MULTI   "
               DISPLAY "SM209A-TEST-1 PASS"
           ELSE
               DISPLAY "SM209A-TEST-1 FAIL"
               DISPLAY "  FIRST=>" WS-FIRST "<"
           END-IF.
      * Test 2: Second replacement pair - numeric field
           IF WS-SECOND = 5678
               DISPLAY "SM209A-TEST-2 PASS"
           ELSE
               DISPLAY "SM209A-TEST-2 FAIL"
               DISPLAY "  SECOND=" WS-SECOND
           END-IF.
      * Test 3: Third and fourth replacement pairs
           IF WS-THIRD = "ABCDE" AND WS-FOURTH = 999999
               DISPLAY "SM209A-TEST-3 PASS"
           ELSE
               DISPLAY "SM209A-TEST-3 FAIL"
               DISPLAY "  THIRD=>" WS-THIRD "<"
               DISPLAY "  FOURTH=" WS-FOURTH
           END-IF.
           STOP RUN.
