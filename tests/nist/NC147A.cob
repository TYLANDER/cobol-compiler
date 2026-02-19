       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC147A.
      *
      * NIST CCVS-style test: Level 88 condition names
      * Tests level 88 items with SET TRUE and VALUE THRU
      * for condition-based branching.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STATUS        PIC 9(2) VALUE ZEROS.
           88 STATUS-ACTIVE   VALUE 1.
           88 STATUS-INACTIVE VALUE 0.
           88 STATUS-PENDING  VALUE 2.
       01 WS-RANGE          PIC 9(3) VALUE ZEROS.
           88 RANGE-LOW       VALUE 1 THRU 10.
           88 RANGE-MED       VALUE 11 THRU 50.
           88 RANGE-HIGH      VALUE 51 THRU 999.
       PROCEDURE DIVISION.
      * Test 1: SET condition-name TO TRUE
      *   SET STATUS-ACTIVE TO TRUE should make WS-STATUS = 1
      *   Then STATUS-ACTIVE should be true
           MOVE 0 TO WS-STATUS.
           SET STATUS-ACTIVE TO TRUE.
           IF STATUS-ACTIVE
               DISPLAY "NC147A-TEST-1 PASS"
           ELSE
               DISPLAY "NC147A-TEST-1 FAIL"
               DISPLAY "  Expected STATUS=1, got " WS-STATUS
           END-IF.
      * Test 2: Level 88 with VALUE THRU (range check)
      *   MOVE 25 => RANGE-MED should be true
      *   MOVE 75 => RANGE-HIGH should be true
           MOVE 25 TO WS-RANGE.
           IF RANGE-MED
               MOVE 75 TO WS-RANGE
               IF RANGE-HIGH
                   DISPLAY "NC147A-TEST-2 PASS"
               ELSE
                   DISPLAY "NC147A-TEST-2 FAIL"
                   DISPLAY "  RANGE-HIGH false for 75"
               END-IF
           ELSE
               DISPLAY "NC147A-TEST-2 FAIL"
               DISPLAY "  RANGE-MED false for 25"
           END-IF.
           STOP RUN.
