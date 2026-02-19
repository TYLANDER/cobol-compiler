       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC145A.
      *
      * NIST CCVS-style test: REDEFINES and group moves
      * Tests the REDEFINES clause to overlay storage and
      * group-level MOVE to copy structured data.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATE-NUM     PIC 9(8) VALUE 20251231.
       01 WS-DATE-X REDEFINES WS-DATE-NUM.
           05 WS-YEAR     PIC X(4).
           05 WS-MONTH    PIC X(2).
           05 WS-DAY      PIC X(2).
       01 WS-GROUP-A.
           05 WS-A1       PIC X(3) VALUE "ABC".
           05 WS-A2       PIC X(3) VALUE "DEF".
       01 WS-GROUP-B.
           05 WS-B1       PIC X(3) VALUE SPACES.
           05 WS-B2       PIC X(3) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: REDEFINES — access parts of a numeric field
      *   WS-DATE-NUM = 20251231
      *   Via REDEFINES: YEAR="2025", MONTH="12", DAY="31"
           IF WS-YEAR = "2025" AND WS-MONTH = "12"
               AND WS-DAY = "31"
               DISPLAY "NC145A-TEST-1 PASS"
           ELSE
               DISPLAY "NC145A-TEST-1 FAIL"
               DISPLAY "  YEAR=>" WS-YEAR "< MONTH=>"
                   WS-MONTH "< DAY=>" WS-DAY "<"
           END-IF.
      * Test 2: Group MOVE copies entire group
      *   MOVE WS-GROUP-A TO WS-GROUP-B
      *   B1 should be "ABC", B2 should be "DEF"
           MOVE WS-GROUP-A TO WS-GROUP-B.
           IF WS-B1 = "ABC" AND WS-B2 = "DEF"
               DISPLAY "NC145A-TEST-2 PASS"
           ELSE
               DISPLAY "NC145A-TEST-2 FAIL"
               DISPLAY "  B1=>" WS-B1 "< B2=>" WS-B2 "<"
           END-IF.
           STOP RUN.
