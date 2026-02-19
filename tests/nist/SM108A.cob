       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM108A.
      *
      * NIST CCVS-style test: COPY with multiple copybooks
      * Tests including two different copybooks in the same program.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM-ALPHA.
       COPY SM-NUMER.
       PROCEDURE DIVISION.
      * Test 1: Both copied fields have correct initial values
           IF SM-A-FIELD = "ALPHA" AND SM-N-FIELD = 5555
               DISPLAY "SM108A-TEST-1 PASS"
           ELSE
               DISPLAY "SM108A-TEST-1 FAIL"
               DISPLAY "  A=>" SM-A-FIELD "<"
               DISPLAY "  N=" SM-N-FIELD
           END-IF.
      * Test 2: Modify both and verify
           MOVE "BRAVO" TO SM-A-FIELD.
           MOVE 1234 TO SM-N-FIELD.
           IF SM-A-FIELD = "BRAVO" AND SM-N-FIELD = 1234
               DISPLAY "SM108A-TEST-2 PASS"
           ELSE
               DISPLAY "SM108A-TEST-2 FAIL"
               DISPLAY "  A=>" SM-A-FIELD "<"
               DISPLAY "  N=" SM-N-FIELD
           END-IF.
           STOP RUN.
