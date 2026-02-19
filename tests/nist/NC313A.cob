       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC313A.
      *
      * NIST CCVS-style test: INSPECT TALLYING CHARACTERS
      * Tests INSPECT TALLYING FOR CHARACTERS with BEFORE
      * and AFTER phrases to count character positions.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA         PIC X(20) VALUE SPACES.
       01 WS-TALLY        PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
       NC313A-CONTROL.
           PERFORM NC313A-TEST-1.
           PERFORM NC313A-TEST-2.
           PERFORM NC313A-TEST-3.
           STOP RUN.
       NC313A-TEST-1.
      * INSPECT TALLYING CHARACTERS (no qualifier)
      *   "ABCDEFGHIJ" is 10 chars in a PIC X(20) field
      *   Total characters in the field = 20
           MOVE "ABCDEFGHIJ" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR CHARACTERS.
           IF WS-TALLY = 20
               DISPLAY "NC313A-TEST-1 PASS"
           ELSE
               DISPLAY "NC313A-TEST-1 FAIL"
               DISPLAY "  Expected 20, got " WS-TALLY
           END-IF.
       NC313A-TEST-2.
      * INSPECT TALLYING CHARACTERS BEFORE "X"
      *   "HELLO X WORLD" => chars before first "X" = 6
      *   (H,E,L,L,O,space)
           MOVE "HELLO X WORLD" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR CHARACTERS BEFORE "X".
           IF WS-TALLY = 6
               DISPLAY "NC313A-TEST-2 PASS"
           ELSE
               DISPLAY "NC313A-TEST-2 FAIL"
               DISPLAY "  Expected 6, got " WS-TALLY
           END-IF.
       NC313A-TEST-3.
      * INSPECT TALLYING CHARACTERS AFTER "D"
      *   "ABCDEFGH" in PIC X(20)
      *   After first "D" at pos 4, remaining = positions 5..20 = 16
           MOVE "ABCDEFGH" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR CHARACTERS AFTER "D".
           IF WS-TALLY = 16
               DISPLAY "NC313A-TEST-3 PASS"
           ELSE
               DISPLAY "NC313A-TEST-3 FAIL"
               DISPLAY "  Expected 16, got " WS-TALLY
           END-IF.
