       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM204A.
      *
      * NIST CCVS-style test: Nested COPY
      * Tests that a copybook containing another COPY statement
      * is correctly expanded (two levels of nesting).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM204A-OUTER.
       PROCEDURE DIVISION.
      * Test 1: Outer copybook field has correct value
           IF SM204A-OUTER-VAL = "OUTER   "
               DISPLAY "SM204A-TEST-1 PASS"
           ELSE
               DISPLAY "SM204A-TEST-1 FAIL"
               DISPLAY "  OUTER=>" SM204A-OUTER-VAL "<"
           END-IF.
      * Test 2: Inner (nested) copybook field has correct value
           IF SM204A-INNER-VAL = "NESTED  "
               DISPLAY "SM204A-TEST-2 PASS"
           ELSE
               DISPLAY "SM204A-TEST-2 FAIL"
               DISPLAY "  INNER=>" SM204A-INNER-VAL "<"
           END-IF.
      * Test 3: Modify both nested fields and verify
           MOVE "MODOUT  " TO SM204A-OUTER-VAL.
           MOVE 5678 TO SM204A-INNER-NUM.
           IF SM204A-OUTER-VAL = "MODOUT  "
             AND SM204A-INNER-NUM = 5678
               DISPLAY "SM204A-TEST-3 PASS"
           ELSE
               DISPLAY "SM204A-TEST-3 FAIL"
               DISPLAY "  OUTER=>" SM204A-OUTER-VAL "<"
               DISPLAY "  NUM=" SM204A-INNER-NUM
           END-IF.
           STOP RUN.
