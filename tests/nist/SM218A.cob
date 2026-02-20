       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM218A.
      *
      * NIST CCVS-style test: NESTED COPY
      * Tests that a copybook can contain a COPY statement
      * (nested COPY).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SMCPY18A.
       PROCEDURE DIVISION.
      * Test 1: VERIFY INNER NESTED FIELD1 FROM SMCPY18B EXISTS
           MOVE "TEST-VALUE" TO WS-NESTED-FIELD1.
           IF WS-NESTED-FIELD1 = "TEST-VALUE"
               DISPLAY "SM218A-TEST-1 PASS"
           ELSE
               DISPLAY "SM218A-TEST-1 FAIL"
               DISPLAY "  Got " WS-NESTED-FIELD1
           END-IF.
      * Test 2: VERIFY INNER NESTED FIELD2 FROM SMCPY18B
           IF WS-NESTED-FIELD2 = 54321
               DISPLAY "SM218A-TEST-2 PASS"
           ELSE
               DISPLAY "SM218A-TEST-2 FAIL"
               DISPLAY "  Expected 54321, got " WS-NESTED-FIELD2
           END-IF.
      * Test 3: VERIFY OUTER FIELD FROM SMCPY18A
           IF WS-OUTER-FIELD = "OUTER-OK"
               DISPLAY "SM218A-TEST-3 PASS"
           ELSE
               DISPLAY "SM218A-TEST-3 FAIL"
               DISPLAY "  Expected OUTER-OK, got " WS-OUTER-FIELD
           END-IF.
           STOP RUN.
