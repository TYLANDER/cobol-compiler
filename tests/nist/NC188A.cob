       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC188A.
      *
      * NIST CCVS-style test: Long string literal handling
      * Tests that the compiler correctly handles string constants
      * of various lengths including near-line-limit strings.
      * (Uses single-line literals rather than continuation syntax.)
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-LONG           PIC X(50) VALUE SPACES.
       01 WS-SHORT          PIC X(10) VALUE SPACES.
       01 WS-TALLY          PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Move a long literal and verify contents
      *   Move a 40-char string and check start and end
           MOVE "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890123"
               TO WS-LONG.
           IF WS-LONG(1:4) = "ABCD"
               AND WS-LONG(27:4) = "0123"
               DISPLAY "NC188A-TEST-1 PASS"
           ELSE
               DISPLAY "NC188A-TEST-1 FAIL"
               DISPLAY "  Start=>" WS-LONG(1:4) "<"
               DISPLAY "  End=>" WS-LONG(27:4) "<"
           END-IF.
      * Test 2: Verify full length of stored string
      *   INSPECT TALLYING CHARACTERS BEFORE INITIAL SPACE
      *   The 40-char string fills positions 1-40
      *   Positions 41-50 should be spaces
           MOVE 0 TO WS-TALLY.
           INSPECT WS-LONG TALLYING WS-TALLY
               FOR CHARACTERS BEFORE INITIAL " ".
           IF WS-TALLY = 40
               DISPLAY "NC188A-TEST-2 PASS"
           ELSE
               DISPLAY "NC188A-TEST-2 FAIL"
               DISPLAY "  Expected tally=40, got " WS-TALLY
           END-IF.
      * Test 3: Concatenation of shorter literals via STRING
      *   Build a known string from parts and verify
           MOVE SPACES TO WS-LONG.
           STRING "HELLO" DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  "WORLD" DELIMITED BY SIZE
               INTO WS-LONG.
           IF WS-LONG(1:11) = "HELLO-WORLD"
               DISPLAY "NC188A-TEST-3 PASS"
           ELSE
               DISPLAY "NC188A-TEST-3 FAIL"
               DISPLAY "  Expected 'HELLO-WORLD'"
               DISPLAY "  Got >" WS-LONG(1:11) "<"
           END-IF.
           STOP RUN.
