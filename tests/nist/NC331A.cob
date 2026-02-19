       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC331A.
      *
      * NIST CCVS-style test: INSPECT TALLYING with BEFORE/AFTER
      * Tests INSPECT TALLYING with multiple BEFORE/AFTER phrases
      * to count characters in bounded regions of a string.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA1       PIC X(20) VALUE SPACES.
       01 WS-DATA2       PIC X(20) VALUE SPACES.
       01 WS-DATA3       PIC X(20) VALUE SPACES.
       01 WS-TALLY       PIC 9(4)  VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: INSPECT TALLYING ALL "A" BEFORE INITIAL "D"
      *   "ABCADEFAG" => A's before first D: A(pos1), A(pos4) => 2
           MOVE "ABCADEFAG" TO WS-DATA1.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA1 TALLYING WS-TALLY
               FOR ALL "A" BEFORE INITIAL "D".
           IF WS-TALLY = 2
               DISPLAY "NC331A-TEST-1 PASS"
           ELSE
               DISPLAY "NC331A-TEST-1 FAIL"
               DISPLAY "  Expected 2, got " WS-TALLY
           END-IF.
      * Test 2: INSPECT TALLYING ALL "A" AFTER INITIAL "D"
      *   "ABCADEFAG" => after first D: E,F,A,G + spaces => 1 A
           MOVE "ABCADEFAG" TO WS-DATA2.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA2 TALLYING WS-TALLY
               FOR ALL "A" AFTER INITIAL "D".
           IF WS-TALLY = 1
               DISPLAY "NC331A-TEST-2 PASS"
           ELSE
               DISPLAY "NC331A-TEST-2 FAIL"
               DISPLAY "  Expected 1, got " WS-TALLY
           END-IF.
      * Test 3: INSPECT TALLYING ALL "B" AFTER "A" BEFORE "F"
      *   "XABCBDEFBX" => after first A, before first F:
      *   region is "BCBDE" => 2 B's
           MOVE "XABCBDEFBX" TO WS-DATA3.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA3 TALLYING WS-TALLY
               FOR ALL "B" AFTER INITIAL "A"
                        BEFORE INITIAL "F".
           IF WS-TALLY = 2
               DISPLAY "NC331A-TEST-3 PASS"
           ELSE
               DISPLAY "NC331A-TEST-3 FAIL"
               DISPLAY "  Expected 2, got " WS-TALLY
           END-IF.
           STOP RUN.
