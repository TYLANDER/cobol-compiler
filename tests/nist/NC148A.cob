       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC148A.
      *
      * NIST CCVS-style test: INSPECT TALLYING LEADING/TRAILING
      * Tests INSPECT TALLYING with LEADING and TRAILING options
      * to count characters at the start or end of a field.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA         PIC X(20) VALUE SPACES.
       01 WS-TALLY        PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: INSPECT TALLYING LEADING
      *   "000123" has 3 leading zeros
           MOVE "000123" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR LEADING "0".
           IF WS-TALLY = 3
               DISPLAY "NC148A-TEST-1 PASS"
           ELSE
               DISPLAY "NC148A-TEST-1 FAIL"
               DISPLAY "  Expected TALLY=3, got " WS-TALLY
           END-IF.
      * Test 2: INSPECT TALLYING TRAILING
      *   "HELLO     " (with trailing spaces padded in PIC X(20))
      *   has trailing spaces; count trailing " " in full field
      *   "HELLO" is 5 chars, field is 20 => 15 trailing spaces
           MOVE "HELLO" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR TRAILING " ".
           IF WS-TALLY = 15
               DISPLAY "NC148A-TEST-2 PASS"
           ELSE
               DISPLAY "NC148A-TEST-2 FAIL"
               DISPLAY "  Expected TALLY=15, got " WS-TALLY
           END-IF.
           STOP RUN.
