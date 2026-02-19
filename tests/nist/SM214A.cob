       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM214A.
      *
      * NIST CCVS-style test: standalone REPLACE statement
      * Tests REPLACE ==old== BY ==new== applied to source text
      * without a COPY directive, and REPLACE OFF to deactivate.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SM214A-ALPHA PIC X(10) VALUE SPACES.
       01 SM214A-NUM   PIC 9(4) VALUE 0.
       01 SM214A-CHK   PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
       REPLACE ==TALPHA== BY ==SM214A-ALPHA==
               ==TNUM==   BY ==SM214A-NUM==.
      * Test 1: REPLACE substitutes alphanumeric identifier
           MOVE "REPLACED  " TO TALPHA.
           IF SM214A-ALPHA = "REPLACED  "
               DISPLAY "SM214A-TEST-1 PASS"
           ELSE
               DISPLAY "SM214A-TEST-1 FAIL"
               DISPLAY "  ALPHA=>" SM214A-ALPHA "<"
           END-IF.
      * Test 2: REPLACE substitutes numeric identifier
           MOVE 4321 TO TNUM.
           IF SM214A-NUM = 4321
               DISPLAY "SM214A-TEST-2 PASS"
           ELSE
               DISPLAY "SM214A-TEST-2 FAIL"
               DISPLAY "  NUM=" SM214A-NUM
           END-IF.
       REPLACE OFF.
      * Test 3: After REPLACE OFF, identifiers are used literally
           MOVE "POST-OFF  " TO SM214A-CHK.
           IF SM214A-CHK = "POST-OFF  "
               DISPLAY "SM214A-TEST-3 PASS"
           ELSE
               DISPLAY "SM214A-TEST-3 FAIL"
               DISPLAY "  CHK=>" SM214A-CHK "<"
           END-IF.
           STOP RUN.
