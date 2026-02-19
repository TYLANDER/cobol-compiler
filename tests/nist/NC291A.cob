       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC291A.
      *
      * NIST CCVS-style test: INSPECT TALLYING with BEFORE/AFTER
      * Tests INSPECT TALLYING with positional qualifiers
      * (BEFORE INITIAL and AFTER INITIAL).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA         PIC X(15) VALUE SPACES.
       01 WS-TALLY        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: INSPECT TALLYING ALL "A" BEFORE INITIAL "X"
      *   "AABAXAAYAA" => region before "X" is "AABA"
      *   Count of "A" before "X" = 3
           MOVE "AABAXAAYAA" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR ALL "A" BEFORE INITIAL "X".
           IF WS-TALLY = 3
               DISPLAY "NC291A-TEST-1 PASS"
           ELSE
               DISPLAY "NC291A-TEST-1 FAIL"
               DISPLAY "  Expected 3, got " WS-TALLY
           END-IF.
      * Test 2: INSPECT TALLYING ALL "B" AFTER INITIAL "X"
      *   "AABXBBCBB" => region after "X" is "BBCBB"
      *   Count of "B" after "X" = 4
           MOVE "AABXBBCBB" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR ALL "B" AFTER INITIAL "X".
           IF WS-TALLY = 4
               DISPLAY "NC291A-TEST-2 PASS"
           ELSE
               DISPLAY "NC291A-TEST-2 FAIL"
               DISPLAY "  Expected 4, got " WS-TALLY
           END-IF.
      * Test 3: INSPECT TALLYING LEADING "0" BEFORE INITIAL "5"
      *   "00050300" => region before "5" is "000"
      *   Leading "0"s before "5" = 3
           MOVE "00050300" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR LEADING "0" BEFORE INITIAL "5".
           IF WS-TALLY = 3
               DISPLAY "NC291A-TEST-3 PASS"
           ELSE
               DISPLAY "NC291A-TEST-3 FAIL"
               DISPLAY "  Expected 3, got " WS-TALLY
           END-IF.
           STOP RUN.
