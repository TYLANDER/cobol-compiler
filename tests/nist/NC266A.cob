       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC266A.
      *
      * NIST CCVS-style test: INSPECT TALLYING with multiple
      * BEFORE/AFTER phrases.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA            PIC X(30) VALUE SPACES.
       01 WS-TALLY           PIC 9(4)  VALUE ZEROS.
       01 WS-TALLY2          PIC 9(4)  VALUE ZEROS.
       01 WS-TALLY3          PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
       NC266A-CONTROL.
           PERFORM NC266A-TEST-1
           PERFORM NC266A-TEST-2
           PERFORM NC266A-TEST-3
           STOP RUN.
       NC266A-TEST-1.
      * INSPECT TALLYING ALL "A" BEFORE INITIAL "X"
      *   "ABACAXYZ" has A at positions 1,3,5 before X at pos 6
      *   Count of A before X = 3
           MOVE "ABACAXYZ" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR ALL "A" BEFORE INITIAL "X".
           IF WS-TALLY = 3
               DISPLAY "NC266A-TEST-1 PASS"
           ELSE
               DISPLAY "NC266A-TEST-1 FAIL"
               DISPLAY "  TALLY=" WS-TALLY
           END-IF.
       NC266A-TEST-2.
      * INSPECT TALLYING ALL "B" AFTER INITIAL "X"
      *   "XXBABBXBB" first X is at pos 1
      *   after it: "XBABBXBB" => B count = 5
           MOVE "XXBABBXBB" TO WS-DATA.
           MOVE 0 TO WS-TALLY2.
           INSPECT WS-DATA TALLYING WS-TALLY2
               FOR ALL "B" AFTER INITIAL "X".
           IF WS-TALLY2 = 5
               DISPLAY "NC266A-TEST-2 PASS"
           ELSE
               DISPLAY "NC266A-TEST-2 FAIL"
               DISPLAY "  TALLY2=" WS-TALLY2
           END-IF.
       NC266A-TEST-3.
      * INSPECT TALLYING CHARACTERS BEFORE INITIAL "Z"
      *   "HELLO WORLD Z" has 12 chars before Z
           MOVE "HELLO WORLD Z" TO WS-DATA.
           MOVE 0 TO WS-TALLY3.
           INSPECT WS-DATA TALLYING WS-TALLY3
               FOR CHARACTERS BEFORE INITIAL "Z".
           IF WS-TALLY3 = 12
               DISPLAY "NC266A-TEST-3 PASS"
           ELSE
               DISPLAY "NC266A-TEST-3 FAIL"
               DISPLAY "  TALLY3=" WS-TALLY3
           END-IF.
