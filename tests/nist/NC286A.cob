       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC286A.
      *
      * NIST CCVS-style test: IF comparing alphanumeric fields
      * of different lengths. Tests that shorter fields are
      * padded with trailing spaces during comparison.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SHORT         PIC X(3) VALUE SPACES.
       01 WS-LONG          PIC X(8) VALUE SPACES.
       01 WS-MED           PIC X(5) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Short field equals long field (space padding)
      *   "ABC" in X(3) compared to "ABC     " in X(8)
      *   They should be EQUAL because trailing spaces match
           MOVE "ABC" TO WS-SHORT.
           MOVE "ABC" TO WS-LONG.
           IF WS-SHORT = WS-LONG
               DISPLAY "NC286A-TEST-1 PASS"
           ELSE
               DISPLAY "NC286A-TEST-1 FAIL"
               DISPLAY "  SHORT=>" WS-SHORT "<"
               DISPLAY "  LONG=>" WS-LONG "<"
           END-IF.
      * Test 2: Short field less than long field
      *   "ABC" in X(3) compared to "ABCZZZZZ" in X(8)
      *   "ABC     " < "ABCZZZZZ" because space < "Z"
           MOVE "ABC" TO WS-SHORT.
           MOVE "ABCZZZZZ" TO WS-LONG.
           IF WS-SHORT < WS-LONG
               DISPLAY "NC286A-TEST-2 PASS"
           ELSE
               DISPLAY "NC286A-TEST-2 FAIL"
               DISPLAY "  SHORT=>" WS-SHORT "<"
               DISPLAY "  LONG=>" WS-LONG "<"
           END-IF.
      * Test 3: Medium field compared to long field
      *   "HELLO" in X(5) vs "HELLO   " in X(8)
      *   Should be EQUAL
           MOVE "HELLO" TO WS-MED.
           MOVE "HELLO" TO WS-LONG.
           IF WS-MED = WS-LONG
               DISPLAY "NC286A-TEST-3 PASS"
           ELSE
               DISPLAY "NC286A-TEST-3 FAIL"
               DISPLAY "  MED=>" WS-MED "<"
               DISPLAY "  LONG=>" WS-LONG "<"
           END-IF.
           STOP RUN.
