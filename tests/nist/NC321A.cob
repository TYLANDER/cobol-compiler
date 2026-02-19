       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC321A.
      *
      * NIST CCVS-style test: Level-88 condition names
      * Tests level-88 with multiple VALUES and THRU ranges.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STATUS     PIC 9(2) VALUE 0.
          88 WS-VALID-SINGLE   VALUE 1 3 5 7 9.
          88 WS-IN-RANGE        VALUE 10 THRU 20.
          88 WS-MIXED           VALUE 1 THRU 5 8 10 THRU 15.
       PROCEDURE DIVISION.
      * Test 1: Level-88 with multiple discrete VALUES
      *   Setting WS-STATUS to 5 should make WS-VALID-SINGLE true.
           MOVE 5 TO WS-STATUS.
           IF WS-VALID-SINGLE
               DISPLAY "NC321A-TEST-1 PASS"
           ELSE
               DISPLAY "NC321A-TEST-1 FAIL"
               DISPLAY "  WS-STATUS=5, expected VALID-SINGLE true"
           END-IF.
      * Test 2: Level-88 with THRU range
      *   Setting WS-STATUS to 15 should make WS-IN-RANGE true.
           MOVE 15 TO WS-STATUS.
           IF WS-IN-RANGE
               DISPLAY "NC321A-TEST-2 PASS"
           ELSE
               DISPLAY "NC321A-TEST-2 FAIL"
               DISPLAY "  WS-STATUS=15, expected IN-RANGE true"
           END-IF.
      * Test 3: Level-88 with mixed VALUES and THRU
      *   Setting WS-STATUS to 8 should make WS-MIXED true.
      *   Setting WS-STATUS to 6 should make WS-MIXED false.
           MOVE 8 TO WS-STATUS.
           IF WS-MIXED
               MOVE 6 TO WS-STATUS
               IF WS-MIXED
                   DISPLAY "NC321A-TEST-3 FAIL"
                   DISPLAY "  WS-STATUS=6, MIXED should be false"
               ELSE
                   DISPLAY "NC321A-TEST-3 PASS"
               END-IF
           ELSE
               DISPLAY "NC321A-TEST-3 FAIL"
               DISPLAY "  WS-STATUS=8, expected MIXED true"
           END-IF.
           STOP RUN.
