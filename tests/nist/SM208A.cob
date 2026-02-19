       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM208A.
      *
      * NIST CCVS-style test: REPLACE statement with pseudo-text
      * Tests that the REPLACE statement with ==old== BY ==new==
      * pseudo-text delimiters substitutes identifiers in source.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM208A-CPY.
       PROCEDURE DIVISION.
       REPLACE ==TVAL== BY ==SM208A-VAL==
               ==TNUM== BY ==SM208A-NUM==.
      * Test 1: REPLACE pseudo-text for alphanumeric identifier
           MOVE "REPLACED  " TO TVAL.
           IF SM208A-VAL = "REPLACED  "
               DISPLAY "SM208A-TEST-1 PASS"
           ELSE
               DISPLAY "SM208A-TEST-1 FAIL"
               DISPLAY "  VAL=>" SM208A-VAL "<"
           END-IF.
      * Test 2: REPLACE pseudo-text for numeric identifier
           MOVE 7777 TO TNUM.
           IF SM208A-NUM = 7777
               DISPLAY "SM208A-TEST-2 PASS"
           ELSE
               DISPLAY "SM208A-TEST-2 FAIL"
               DISPLAY "  NUM=" SM208A-NUM
           END-IF.
       REPLACE OFF.
      * Test 3: After REPLACE OFF, use real names directly
           MOVE "AFTER-OFF " TO SM208A-CHK.
           IF SM208A-CHK = "AFTER-OFF "
               DISPLAY "SM208A-TEST-3 PASS"
           ELSE
               DISPLAY "SM208A-TEST-3 FAIL"
               DISPLAY "  CHK=>" SM208A-CHK "<"
           END-IF.
           STOP RUN.
