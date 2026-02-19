       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM201A.
      *
      * NIST CCVS-style test: COPY with REPLACING of data names
      * Tests that COPY REPLACING substitutes data-name tokens.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM201A-CPY REPLACING ==ORIG-ALPHA== BY ==WS-ALPHA==
                                  ==ORIG-NUM== BY ==WS-NUM==
                                  ==ORIG-FLAG== BY ==WS-FLAG==.
       PROCEDURE DIVISION.
      * Test 1: Replaced data names have correct initial values
           IF WS-ALPHA = "DATA-REPL " AND WS-NUM = 4567
               DISPLAY "SM201A-TEST-1 PASS"
           ELSE
               DISPLAY "SM201A-TEST-1 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA "<"
               DISPLAY "  NUM=" WS-NUM
           END-IF.
      * Test 2: MOVE to replaced data names and verify
           MOVE "NEWVALUE  " TO WS-ALPHA.
           MOVE 9999 TO WS-NUM.
           IF WS-ALPHA = "NEWVALUE  " AND WS-NUM = 9999
               DISPLAY "SM201A-TEST-2 PASS"
           ELSE
               DISPLAY "SM201A-TEST-2 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA "<"
               DISPLAY "  NUM=" WS-NUM
           END-IF.
      * Test 3: Flag field replacement works
           IF WS-FLAG = 1
               DISPLAY "SM201A-TEST-3 PASS"
           ELSE
               DISPLAY "SM201A-TEST-3 FAIL"
               DISPLAY "  FLAG=" WS-FLAG
           END-IF.
           STOP RUN.
