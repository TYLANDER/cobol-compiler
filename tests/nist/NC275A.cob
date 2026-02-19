       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC275A.
      *
      * NIST CCVS-style test: INSPECT TALLYING variations
      * Tests INSPECT TALLYING ALL, LEADING, and CHARACTERS
      * with BEFORE/AFTER phrases.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA        PIC X(20) VALUE SPACES.
       01 WS-TALLY       PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
       NC275A-CONTROL.
           PERFORM NC275A-TEST-1.
           PERFORM NC275A-TEST-2.
           PERFORM NC275A-TEST-3.
           STOP RUN.
       NC275A-TEST-1.
      * INSPECT TALLYING ALL "A" in "ABRACADABRA"
      *   A appears at positions 1,4,6,8,11 => 5 occurrences
           MOVE "ABRACADABRA" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR ALL "A".
           IF WS-TALLY = 5
               DISPLAY "NC275A-TEST-1 PASS"
           ELSE
               DISPLAY "NC275A-TEST-1 FAIL"
               DISPLAY "  Expected 5, got " WS-TALLY
           END-IF.
       NC275A-TEST-2.
      * INSPECT TALLYING LEADING "0" in "000123"
      *   Leading zeros: 3
           MOVE "000123" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR LEADING "0".
           IF WS-TALLY = 3
               DISPLAY "NC275A-TEST-2 PASS"
           ELSE
               DISPLAY "NC275A-TEST-2 FAIL"
               DISPLAY "  Expected 3, got " WS-TALLY
           END-IF.
       NC275A-TEST-3.
      * INSPECT TALLYING CHARACTERS BEFORE "X"
      *   "HELLO X WORLD" => characters before first "X" = 6
      *   (H, E, L, L, O, space)
           MOVE "HELLO X WORLD" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR CHARACTERS BEFORE "X".
           IF WS-TALLY = 6
               DISPLAY "NC275A-TEST-3 PASS"
           ELSE
               DISPLAY "NC275A-TEST-3 FAIL"
               DISPLAY "  Expected 6, got " WS-TALLY
           END-IF.
