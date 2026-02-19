       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC108A.
      *
      * NIST CCVS-style test: MOVE statement variations
      * Tests MOVE with different data sizes, truncation,
      * alphanumeric moves, SPACES, and ZEROS.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SMALL    PIC 9(2) VALUE ZEROS.
       01 WS-MED      PIC 9(4) VALUE ZEROS.
       01 WS-BIG      PIC 9(8) VALUE ZEROS.
       01 WS-ALPHA    PIC X(10) VALUE SPACES.
       01 WS-NUM-TST  PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: MOVE numeric literal to PIC 9(2)
           MOVE 42 TO WS-SMALL.
           IF WS-SMALL = 42
               DISPLAY "NC108A-TEST-1 PASS"
           ELSE
               DISPLAY "NC108A-TEST-1 FAIL"
               DISPLAY "  Expected 42, got " WS-SMALL
           END-IF.
      * Test 2: MOVE numeric literal to PIC 9(4)
           MOVE 1234 TO WS-MED.
           IF WS-MED = 1234
               DISPLAY "NC108A-TEST-2 PASS"
           ELSE
               DISPLAY "NC108A-TEST-2 FAIL"
               DISPLAY "  Expected 1234, got " WS-MED
           END-IF.
      * Test 3: MOVE numeric literal to PIC 9(8)
           MOVE 12345678 TO WS-BIG.
           IF WS-BIG = 12345678
               DISPLAY "NC108A-TEST-3 PASS"
           ELSE
               DISPLAY "NC108A-TEST-3 FAIL"
               DISPLAY "  Expected 12345678, got " WS-BIG
           END-IF.
      * Test 4: MOVE with truncation (large value to small field)
           MOVE 999 TO WS-SMALL.
           IF WS-SMALL = 99
               DISPLAY "NC108A-TEST-4 PASS"
           ELSE
               DISPLAY "NC108A-TEST-4 FAIL"
               DISPLAY "  Expected 99, got " WS-SMALL
           END-IF.
      * Test 5: MOVE alphanumeric literal to alphanumeric field
           MOVE "HELLO     " TO WS-ALPHA.
           IF WS-ALPHA = "HELLO     "
               DISPLAY "NC108A-TEST-5 PASS"
           ELSE
               DISPLAY "NC108A-TEST-5 FAIL"
               DISPLAY "  Expected HELLO     , got " WS-ALPHA
           END-IF.
      * Test 6: MOVE SPACES to alphanumeric field
           MOVE "XXXXXXXXXX" TO WS-ALPHA.
           MOVE SPACES TO WS-ALPHA.
           IF WS-ALPHA = SPACES
               DISPLAY "NC108A-TEST-6 PASS"
           ELSE
               DISPLAY "NC108A-TEST-6 FAIL"
               DISPLAY "  Expected SPACES, got " WS-ALPHA
           END-IF.
      * Test 7: MOVE ZEROS to numeric field
           MOVE 9999 TO WS-NUM-TST.
           MOVE ZEROS TO WS-NUM-TST.
           IF WS-NUM-TST = 0
               DISPLAY "NC108A-TEST-7 PASS"
           ELSE
               DISPLAY "NC108A-TEST-7 FAIL"
               DISPLAY "  Expected 0, got " WS-NUM-TST
           END-IF.
           STOP RUN.
