       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC124A.
      *
      * NIST CCVS-style test: MOVE type conversion edge cases
      * Tests MOVE numeric to alphanumeric, MOVE SPACES to
      * numeric, and MOVE alphanumeric with spaces to numeric.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM       PIC 9(4) VALUE ZEROS.
       01 WS-ALPHA     PIC X(10) VALUE SPACES.
       01 WS-NUM2      PIC 9(4) VALUE ZEROS.
       01 WS-ALPHA2    PIC X(10) VALUE SPACES.
       01 WS-NUM3      PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: MOVE numeric literal to alphanumeric field
      *   Numeric 1234 moved to PIC X(10) should be left-justified
      *   and space-padded on the right
           MOVE "1234" TO WS-ALPHA.
           IF WS-ALPHA = "1234      "
               DISPLAY "NC124A-TEST-1 PASS"
           ELSE
               DISPLAY "NC124A-TEST-1 FAIL"
               DISPLAY "  Expected >1234      <, got >"
                   WS-ALPHA "<"
           END-IF.
      * Test 2: MOVE ZEROS to numeric field then verify value
      *   Start with a non-zero value, move ZEROS, confirm zero
           MOVE 5678 TO WS-NUM2.
           MOVE ZEROS TO WS-NUM2.
           IF WS-NUM2 = 0
               DISPLAY "NC124A-TEST-2 PASS"
           ELSE
               DISPLAY "NC124A-TEST-2 FAIL"
               DISPLAY "  Expected 0, got " WS-NUM2
           END-IF.
      * Test 3: MOVE alphanumeric digit string to numeric field
      *   Move "0042" (alphanumeric containing digits) to PIC 9(4)
           MOVE "0042" TO WS-ALPHA2.
           MOVE WS-ALPHA2 TO WS-NUM3.
           IF WS-NUM3 = 42
               DISPLAY "NC124A-TEST-3 PASS"
           ELSE
               DISPLAY "NC124A-TEST-3 FAIL"
               DISPLAY "  Expected 42, got " WS-NUM3
           END-IF.
           STOP RUN.
