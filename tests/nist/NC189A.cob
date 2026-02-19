       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC189A.
      *
      * NIST CCVS-style test: INSPECT TALLYING ALL and CHARACTERS
      * Tests INSPECT TALLYING with ALL for multi-character patterns
      * and CHARACTERS count for total character counting.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA           PIC X(30) VALUE SPACES.
       01 WS-TALLY          PIC 9(4)  VALUE ZEROS.
       01 WS-TALLY2         PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: INSPECT TALLYING ALL "AB"
      *   "ABABXABYYAB" has 4 occurrences of "AB"
           MOVE "ABABXABYYAB" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR ALL "AB".
           IF WS-TALLY = 4
               DISPLAY "NC189A-TEST-1 PASS"
           ELSE
               DISPLAY "NC189A-TEST-1 FAIL"
               DISPLAY "  Expected 4, got " WS-TALLY
           END-IF.
      * Test 2: INSPECT TALLYING CHARACTERS
      *   Count all characters (entire field length = 30)
           MOVE "HELLO WORLD" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR CHARACTERS.
           IF WS-TALLY = 30
               DISPLAY "NC189A-TEST-2 PASS"
           ELSE
               DISPLAY "NC189A-TEST-2 FAIL"
               DISPLAY "  Expected 30, got " WS-TALLY
           END-IF.
      * Test 3: INSPECT TALLYING CHARACTERS BEFORE INITIAL space
      *   "ABCDEF GHIJ" => 6 characters before first space
           MOVE "ABCDEF GHIJ" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR CHARACTERS BEFORE INITIAL " ".
           IF WS-TALLY = 6
               DISPLAY "NC189A-TEST-3 PASS"
           ELSE
               DISPLAY "NC189A-TEST-3 FAIL"
               DISPLAY "  Expected 6, got " WS-TALLY
           END-IF.
           STOP RUN.
