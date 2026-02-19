       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM215A.
      *
      * NIST CCVS-style test: COPY with IN library clause
      * Tests that COPY ... IN library-name syntax is accepted
      * and the copybook is resolved. Also combines with REPLACING
      * to verify both features work together with the IN clause.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM215A-CPY IN NISTLIB
           REPLACING ==:PFX:== BY ==WS==
                     ==:LIT:== BY =="LIBCOPY   "==
                     ==:NUM:== BY ==3456==.
       PROCEDURE DIVISION.
      * Test 1: COPY IN with REPLACING - alpha field
           IF WS-ITEM = "LIBCOPY   "
               DISPLAY "SM215A-TEST-1 PASS"
           ELSE
               DISPLAY "SM215A-TEST-1 FAIL"
               DISPLAY "  ITEM=>" WS-ITEM "<"
           END-IF.
      * Test 2: COPY IN with REPLACING - numeric field
           IF WS-COUNT = 3456
               DISPLAY "SM215A-TEST-2 PASS"
           ELSE
               DISPLAY "SM215A-TEST-2 FAIL"
               DISPLAY "  COUNT=" WS-COUNT
           END-IF.
      * Test 3: Modify and verify the fields are usable
           MOVE "MODIFIED  " TO WS-ITEM.
           MOVE 9999 TO WS-COUNT.
           IF WS-ITEM = "MODIFIED  " AND WS-COUNT = 9999
               DISPLAY "SM215A-TEST-3 PASS"
           ELSE
               DISPLAY "SM215A-TEST-3 FAIL"
               DISPLAY "  ITEM=>" WS-ITEM "<"
               DISPLAY "  COUNT=" WS-COUNT
           END-IF.
           STOP RUN.
