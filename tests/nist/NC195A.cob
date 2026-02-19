       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC195A.
      *
      * NIST CCVS-style test: INSPECT TALLYING with BEFORE/AFTER
      * Tests INSPECT TALLYING combined with BEFORE INITIAL and
      * AFTER INITIAL phrases to count characters in specific
      * regions of a string.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA       PIC X(20) VALUE SPACES.
       01 WS-TALLY      PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: INSPECT TALLYING ALL "A" BEFORE INITIAL "X"
      *   "ABACAXDEF" - count A's before the first X
      *   A at pos 1, A at pos 3, A at pos 5 => 3 A's before X at pos 6
           MOVE "ABACAXDEF" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR ALL "A" BEFORE INITIAL "X".
           IF WS-TALLY = 3
               DISPLAY "NC195A-TEST-1 PASS"
           ELSE
               DISPLAY "NC195A-TEST-1 FAIL"
               DISPLAY "  Expected 3, got " WS-TALLY
           END-IF.
      * Test 2: INSPECT TALLYING ALL "B" AFTER INITIAL "X"
      *   "ABCXBBDB" - count B's after the first X
      *   X is at pos 4; after X: "BBDB" has B at 5, B at 6, B at 8 => 3
           MOVE "ABCXBBDB" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR ALL "B" AFTER INITIAL "X".
           IF WS-TALLY = 3
               DISPLAY "NC195A-TEST-2 PASS"
           ELSE
               DISPLAY "NC195A-TEST-2 FAIL"
               DISPLAY "  Expected 3, got " WS-TALLY
           END-IF.
      * Test 3: INSPECT TALLYING CHARACTERS BEFORE INITIAL
      *   "HELLO*WORLD" - count all characters before first "*"
      *   Characters before "*": H, E, L, L, O => 5
           MOVE "HELLO*WORLD" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR CHARACTERS BEFORE INITIAL "*".
           IF WS-TALLY = 5
               DISPLAY "NC195A-TEST-3 PASS"
           ELSE
               DISPLAY "NC195A-TEST-3 FAIL"
               DISPLAY "  Expected 5, got " WS-TALLY
           END-IF.
           STOP RUN.
