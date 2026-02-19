       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM107A.
      *
      * NIST CCVS-style test: COPY REPLACING multiple pairs
      * Tests COPY with multiple REPLACING pairs.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM-MULTI
           REPLACING ==OLD-ALPHA== BY ==NEW-ALPHA==
                     ==OLD-BETA== BY ==NEW-BETA==.
       PROCEDURE DIVISION.
      * Test 1: Multiple replacements applied
           IF NEW-ALPHA = "ALPHA" AND NEW-BETA = 9999
               DISPLAY "SM107A-TEST-1 PASS"
           ELSE
               DISPLAY "SM107A-TEST-1 FAIL"
               DISPLAY "  ALPHA=>" NEW-ALPHA "<"
               DISPLAY "  BETA=" NEW-BETA
           END-IF.
      * Test 2: Modify replaced fields
           MOVE "DELTA" TO NEW-ALPHA.
           MOVE 1111 TO NEW-BETA.
           IF NEW-ALPHA = "DELTA" AND NEW-BETA = 1111
               DISPLAY "SM107A-TEST-2 PASS"
           ELSE
               DISPLAY "SM107A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
