       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC314A.
      *
      * NIST CCVS-style test: Alphanumeric comparison with spaces
      * Tests that shorter strings are padded with spaces on the
      * right when compared to longer fields (COBOL standard).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SHORT        PIC X(2)  VALUE SPACES.
       01 WS-LONG         PIC X(5)  VALUE SPACES.
       01 WS-FIELD        PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: PIC X(2) "AB" compared to PIC X(5) "AB   "
      *   "AB" in X(2) equals "AB   " in X(5) due to space padding
           MOVE "AB" TO WS-SHORT.
           MOVE "AB" TO WS-LONG.
           IF WS-SHORT = WS-LONG
               DISPLAY "NC314A-TEST-1 PASS"
           ELSE
               DISPLAY "NC314A-TEST-1 FAIL"
               DISPLAY "  SHORT=>" WS-SHORT "<"
               DISPLAY "  LONG=>" WS-LONG "<"
           END-IF.
      * Test 2: PIC X(10) "HELLO" compared to literal "HELLO"
      *   "HELLO     " in X(10) equals literal "HELLO"
           MOVE "HELLO" TO WS-FIELD.
           IF WS-FIELD = "HELLO"
               DISPLAY "NC314A-TEST-2 PASS"
           ELSE
               DISPLAY "NC314A-TEST-2 FAIL"
               DISPLAY "  FIELD=>" WS-FIELD "<"
           END-IF.
      * Test 3: Non-equal comparison with space padding
      *   "AB" in X(2) compared to "AC" in X(5)
      *   "AB" < "AC" because B < C
           MOVE "AB" TO WS-SHORT.
           MOVE "AC" TO WS-LONG.
           IF WS-SHORT < WS-LONG
               DISPLAY "NC314A-TEST-3 PASS"
           ELSE
               DISPLAY "NC314A-TEST-3 FAIL"
               DISPLAY "  SHORT=>" WS-SHORT "<"
               DISPLAY "  LONG=>" WS-LONG "<"
           END-IF.
           STOP RUN.
