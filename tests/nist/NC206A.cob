       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC206A.
      *
      * NIST CCVS-style test: INSPECT TALLYING and REPLACING
      * Tests INSPECT TALLYING with LEADING, and INSPECT REPLACING
      * ALL with single replacement pairs.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA         PIC X(20) VALUE SPACES.
       01 WS-TALLY        PIC 9(4)  VALUE ZEROS.
       01 WS-TALLY2       PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: INSPECT TALLYING ALL for one character
      *   "ABCAABBC" has 3 A's
           MOVE "ABCAABBC" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING
               WS-TALLY FOR ALL "A".
           IF WS-TALLY = 3
               DISPLAY "NC206A-TEST-1 PASS"
           ELSE
               DISPLAY "NC206A-TEST-1 FAIL"
               DISPLAY "  Expected 3, got " WS-TALLY
           END-IF.
      * Test 2: INSPECT TALLYING LEADING zeros
      *   "000ABC000" has 3 leading zeros
           MOVE "000ABC000" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING
               WS-TALLY FOR LEADING "0".
           IF WS-TALLY = 3
               DISPLAY "NC206A-TEST-2 PASS"
           ELSE
               DISPLAY "NC206A-TEST-2 FAIL"
               DISPLAY "  Expected 3, got " WS-TALLY
           END-IF.
      * Test 3: INSPECT REPLACING ALL - single pair
      *   "AABBCC" replace A by X => "XXBBCC"
           MOVE "AABBCC" TO WS-DATA.
           INSPECT WS-DATA REPLACING
               ALL "A" BY "X".
           IF WS-DATA(1:6) = "XXBBCC"
               DISPLAY "NC206A-TEST-3 PASS"
           ELSE
               DISPLAY "NC206A-TEST-3 FAIL"
               DISPLAY "  Expected XXBBCC, got >"
                   WS-DATA "<"
           END-IF.
           STOP RUN.
