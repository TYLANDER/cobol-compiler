       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC118A.
      *
      * NIST CCVS-style test: INSPECT statement
      * Tests INSPECT TALLYING (count spaces), INSPECT REPLACING
      * (replace characters), and INSPECT CONVERTING (translate).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STRING    PIC X(20) VALUE SPACES.
       01 WS-TALLY     PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: INSPECT TALLYING — count spaces in a string
           MOVE "A B C D E" TO WS-STRING.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-STRING TALLYING WS-TALLY
               FOR ALL " ".
      *   "A B C D E" is 9 chars, the remaining 11 in PIC X(20)
      *   are also spaces. Total spaces = 4 + 11 = 15.
           IF WS-TALLY = 15
               DISPLAY "NC118A-TEST-1 PASS"
           ELSE
               DISPLAY "NC118A-TEST-1 FAIL"
               DISPLAY "  Expected 15, got " WS-TALLY
           END-IF.
      * Test 2: INSPECT REPLACING — replace all "X" by "O"
           MOVE "XOXOXO              " TO WS-STRING.
           INSPECT WS-STRING REPLACING ALL "X" BY "O".
           IF WS-STRING(1:6) = "OOOOOO"
               DISPLAY "NC118A-TEST-2 PASS"
           ELSE
               DISPLAY "NC118A-TEST-2 FAIL"
               DISPLAY "  Expected OOOOOO, got >"
                   WS-STRING "<"
           END-IF.
      * Test 3: INSPECT CONVERTING — translate lowercase to upper
           MOVE "abcdef              " TO WS-STRING.
           INSPECT WS-STRING CONVERTING
               "abcdef" TO "ABCDEF".
           IF WS-STRING(1:6) = "ABCDEF"
               DISPLAY "NC118A-TEST-3 PASS"
           ELSE
               DISPLAY "NC118A-TEST-3 FAIL"
               DISPLAY "  Expected ABCDEF, got >"
                   WS-STRING "<"
           END-IF.
           STOP RUN.
