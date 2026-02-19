       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM211A.
      *
      * NIST CCVS-style test: COPY with nested REPLACING
      * Tests COPY REPLACING with multiple operand pairs that
      * substitute both prefix tags and literal values in a
      * single COPY statement.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM211A-CPY REPLACING ==:TAG1:== BY ==WS-ALPHA==
                                  ==:TAG2:== BY ==WS-NUM==
                                  ==:VAL1:== BY =="MULTI-RPL "==
                                  ==:VAL2:== BY ==5678==
                                  ==:VAL3:== BY =="YES"==.
       PROCEDURE DIVISION.
      * Test 1: First tag replaced alpha has correct initial value
           IF WS-ALPHA-FIELD = "MULTI-RPL "
               DISPLAY "SM211A-TEST-1 PASS"
           ELSE
               DISPLAY "SM211A-TEST-1 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA-FIELD "<"
           END-IF.
      * Test 2: Second tag replaced numeric has correct value
           IF WS-NUM-FIELD = 5678
               DISPLAY "SM211A-TEST-2 PASS"
           ELSE
               DISPLAY "SM211A-TEST-2 FAIL"
               DISPLAY "  NUM=" WS-NUM-FIELD
           END-IF.
      * Test 3: First tag replaced flag has correct value
           IF WS-ALPHA-FLAG = "YES"
               DISPLAY "SM211A-TEST-3 PASS"
           ELSE
               DISPLAY "SM211A-TEST-3 FAIL"
               DISPLAY "  FLAG=>" WS-ALPHA-FLAG "<"
           END-IF.
           STOP RUN.
