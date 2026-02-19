       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM103A.
      *
      * NIST CCVS-style test: Nested COPY
      * Tests COPY of a copybook that itself contains a COPY.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM-NESTED-OUTER.
       PROCEDURE DIVISION.
      * Test 1: Outer copybook data available
           IF SM-OUTER-VAR = "OUTER"
               DISPLAY "SM103A-TEST-1 PASS"
           ELSE
               DISPLAY "SM103A-TEST-1 FAIL"
               DISPLAY "  OUTER=>" SM-OUTER-VAR "<"
           END-IF.
      * Test 2: Inner copybook (nested COPY) data available
           IF SM-INNER-VAR = "INNER"
               DISPLAY "SM103A-TEST-2 PASS"
           ELSE
               DISPLAY "SM103A-TEST-2 FAIL"
               DISPLAY "  INNER=>" SM-INNER-VAR "<"
           END-IF.
      * Test 3: Modify both nested-copy fields
           MOVE "AAAAA" TO SM-OUTER-VAR.
           MOVE "BBBBB" TO SM-INNER-VAR.
           IF SM-OUTER-VAR = "AAAAA" AND SM-INNER-VAR = "BBBBB"
               DISPLAY "SM103A-TEST-3 PASS"
           ELSE
               DISPLAY "SM103A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
