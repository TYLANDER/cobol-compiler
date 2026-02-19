       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM101A.
      *
      * NIST CCVS-style test: COPY statement basics
      * Tests basic COPY expansion of data items.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM-FIELDS.
       PROCEDURE DIVISION.
      * Test 1: COPY expands data items correctly
           IF SM-FIELD-A = "COPY-WORKS"
             AND SM-FIELD-B = 1234
               DISPLAY "SM101A-TEST-1 PASS"
           ELSE
               DISPLAY "SM101A-TEST-1 FAIL"
               DISPLAY "  A=>" SM-FIELD-A "<"
               DISPLAY "  B=" SM-FIELD-B
           END-IF.
      * Test 2: MOVE to/from copied data items
           MOVE "CHANGED   " TO SM-FIELD-A.
           MOVE 5678 TO SM-FIELD-B.
           IF SM-FIELD-A = "CHANGED   "
             AND SM-FIELD-B = 5678
               DISPLAY "SM101A-TEST-2 PASS"
           ELSE
               DISPLAY "SM101A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
