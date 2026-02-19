       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC140A.
      *
      * NIST CCVS-style test: INSPECT statement
      * Tests INSPECT TALLYING and INSPECT REPLACING.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA        PIC X(20) VALUE SPACES.
       01 WS-TALLY       PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: INSPECT TALLYING for occurrences
      *   Count the number of "L" characters in "HELLO WORLD"
           MOVE "HELLO WORLD" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR ALL "L".
           IF WS-TALLY = 3
               DISPLAY "NC140A-TEST-1 PASS"
           ELSE
               DISPLAY "NC140A-TEST-1 FAIL"
               DISPLAY "  Expected TALLY=3, got " WS-TALLY
           END-IF.
      * Test 2: INSPECT REPLACING
      *   Replace all "L" with "X" in "HELLO WORLD"
      *   Result should be "HEXXO WORXD"
           MOVE "HELLO WORLD" TO WS-DATA.
           INSPECT WS-DATA REPLACING ALL "L" BY "X".
           IF WS-DATA(1:11) = "HEXXO WORXD"
               DISPLAY "NC140A-TEST-2 PASS"
           ELSE
               DISPLAY "NC140A-TEST-2 FAIL"
               DISPLAY "  Expected HEXXO WORXD, got >"
                   WS-DATA "<"
           END-IF.
           STOP RUN.
