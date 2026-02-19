       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC337A.
      *
      * NIST CCVS-style test: Alphanumeric comparison rules.
      * Tests that when comparing alphanumeric fields of different
      * lengths, the shorter field is logically padded on the right
      * with spaces per the COBOL-85 standard.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SHORT         PIC X(3) VALUE SPACES.
       01 WS-LONG          PIC X(8) VALUE SPACES.
       01 WS-MEDIUM        PIC X(5) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: "ABC" (3 bytes) compared to "ABC     " (8 bytes)
      *   Shorter field padded with spaces => they should be EQUAL.
           MOVE "ABC" TO WS-SHORT.
           MOVE "ABC     " TO WS-LONG.
           IF WS-SHORT = WS-LONG
               DISPLAY "NC337A-TEST-1 PASS"
           ELSE
               DISPLAY "NC337A-TEST-1 FAIL"
               DISPLAY "  'ABC' should equal 'ABC     '"
           END-IF.
      * Test 2: "ABC" compared to "ABCD    "
      *   "ABC  " < "ABCD " because space < "D"
           MOVE "ABC" TO WS-SHORT.
           MOVE "ABCD    " TO WS-LONG.
           IF WS-SHORT < WS-LONG
               DISPLAY "NC337A-TEST-2 PASS"
           ELSE
               DISPLAY "NC337A-TEST-2 FAIL"
               DISPLAY "  'ABC' should be less than 'ABCD    '"
           END-IF.
      * Test 3: "XYZ  " (5 bytes) compared to "XYZ" (3 bytes)
      *   "XYZ  " vs "XYZ  " (padded) => EQUAL
           MOVE "XYZ  " TO WS-MEDIUM.
           MOVE "XYZ" TO WS-SHORT.
           IF WS-MEDIUM = WS-SHORT
               DISPLAY "NC337A-TEST-3 PASS"
           ELSE
               DISPLAY "NC337A-TEST-3 FAIL"
               DISPLAY "  'XYZ  ' should equal 'XYZ'"
           END-IF.
           STOP RUN.
