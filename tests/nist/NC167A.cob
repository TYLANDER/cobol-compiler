       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC167A.
      *
      * NIST CCVS-style test: REDEFINES clause
      * Tests REDEFINES to overlay storage and interpret the
      * same bytes with different PIC clauses.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RAW          PIC X(8) VALUE "20261231".
       01 WS-DATE-R REDEFINES WS-RAW.
           05 WS-YEAR      PIC X(4).
           05 WS-MONTH     PIC X(2).
           05 WS-DAY       PIC X(2).
       01 WS-NUM-FULL     PIC 9(6) VALUE 123456.
       01 WS-NUM-R REDEFINES WS-NUM-FULL.
           05 WS-HIGH      PIC 9(3).
           05 WS-LOW       PIC 9(3).
       01 WS-ALPHA        PIC X(4) VALUE "ABCD".
       01 WS-ALPHA-R REDEFINES WS-ALPHA.
           05 WS-PAIR1     PIC X(2).
           05 WS-PAIR2     PIC X(2).
       PROCEDURE DIVISION.
      * Test 1: REDEFINES date string into components
      *   "20261231" => YEAR="2026", MONTH="12", DAY="31"
           IF WS-YEAR = "2026" AND WS-MONTH = "12"
               AND WS-DAY = "31"
               DISPLAY "NC167A-TEST-1 PASS"
           ELSE
               DISPLAY "NC167A-TEST-1 FAIL"
               DISPLAY "  YEAR=>" WS-YEAR "<"
               DISPLAY "  MONTH=>" WS-MONTH "<"
               DISPLAY "  DAY=>" WS-DAY "<"
           END-IF.
      * Test 2: REDEFINES numeric into two halves
      *   123456 => HIGH=123, LOW=456
           IF WS-HIGH = 123 AND WS-LOW = 456
               DISPLAY "NC167A-TEST-2 PASS"
           ELSE
               DISPLAY "NC167A-TEST-2 FAIL"
               DISPLAY "  HIGH=" WS-HIGH " LOW=" WS-LOW
           END-IF.
      * Test 3: Modify via REDEFINES and read back
      *   Change PAIR1 to "XY", then WS-ALPHA should be "XYCD"
           MOVE "XY" TO WS-PAIR1.
           IF WS-ALPHA = "XYCD"
               DISPLAY "NC167A-TEST-3 PASS"
           ELSE
               DISPLAY "NC167A-TEST-3 FAIL"
               DISPLAY "  Expected XYCD, got >" WS-ALPHA "<"
           END-IF.
           STOP RUN.
