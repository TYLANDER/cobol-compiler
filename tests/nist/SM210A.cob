       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM210A.
      *
      * NIST CCVS-style test: Nested COPY with REPLACING
      * Tests that an outer COPY with REPLACING includes an inner
      * copybook that also uses REPLACING for its own tokens.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM210A-OUTER REPLACING ==:OUT:== BY ==SM210A-OUT==
                                    ==:OUT-LIT:== BY =="OUTER   "==.
       PROCEDURE DIVISION.
      * Test 1: Outer copybook field has correct replaced value
           IF SM210A-OUT-VAL = "OUTER   "
               DISPLAY "SM210A-TEST-1 PASS"
           ELSE
               DISPLAY "SM210A-TEST-1 FAIL"
               DISPLAY "  OUTER=>" SM210A-OUT-VAL "<"
           END-IF.
      * Test 2: Inner copybook alpha field has correct replaced value
           IF SM210A-VAL = "INNER   "
               DISPLAY "SM210A-TEST-2 PASS"
           ELSE
               DISPLAY "SM210A-TEST-2 FAIL"
               DISPLAY "  INNER=>" SM210A-VAL "<"
           END-IF.
      * Test 3: Inner copybook numeric field has correct replaced value
           IF SM210A-NUM = 4321
               DISPLAY "SM210A-TEST-3 PASS"
           ELSE
               DISPLAY "SM210A-TEST-3 FAIL"
               DISPLAY "  NUM=" SM210A-NUM
           END-IF.
           STOP RUN.
