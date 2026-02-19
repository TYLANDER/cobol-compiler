       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC159A.
      *
      * NIST CCVS-style test: INSPECT REPLACING LEADING/TRAILING/FIRST
      * Tests INSPECT REPLACING with LEADING, TRAILING, and FIRST
      * options to modify specific character occurrences.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA          PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: INSPECT REPLACING LEADING
      *   "000123000" => replace leading "0" by "X" => "XXX123000"
           MOVE "000123000" TO WS-DATA.
           INSPECT WS-DATA REPLACING LEADING "0" BY "X".
           IF WS-DATA(1:9) = "XXX123000"
               DISPLAY "NC159A-TEST-1 PASS"
           ELSE
               DISPLAY "NC159A-TEST-1 FAIL"
               DISPLAY "  Expected XXX123000, got >"
                   WS-DATA "<"
           END-IF.
      * Test 2: INSPECT REPLACING TRAILING
      *   "HELLO  " (in 20-byte field, padded with spaces)
      *   Replace trailing " " by "." => "HELLO..............."
           MOVE "HELLO" TO WS-DATA.
           INSPECT WS-DATA REPLACING TRAILING " " BY ".".
           IF WS-DATA(1:5) = "HELLO"
               AND WS-DATA(6:1) = "."
               DISPLAY "NC159A-TEST-2 PASS"
           ELSE
               DISPLAY "NC159A-TEST-2 FAIL"
               DISPLAY "  Got >" WS-DATA "<"
           END-IF.
      * Test 3: INSPECT REPLACING FIRST
      *   "ABCABC" => replace FIRST "A" by "X" => "XBCABC"
           MOVE "ABCABC" TO WS-DATA.
           INSPECT WS-DATA REPLACING FIRST "A" BY "X".
           IF WS-DATA(1:6) = "XBCABC"
               DISPLAY "NC159A-TEST-3 PASS"
           ELSE
               DISPLAY "NC159A-TEST-3 FAIL"
               DISPLAY "  Expected XBCABC, got >"
                   WS-DATA "<"
           END-IF.
           STOP RUN.
