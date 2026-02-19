       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC115A.
      *
      * NIST CCVS-style test: INSPECT statement
      * Tests INSPECT TALLYING ALL, INSPECT REPLACING ALL,
      * and INSPECT CONVERTING.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STRING   PIC X(20) VALUE SPACES.
       01 WS-TALLY    PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: INSPECT TALLYING ALL
           MOVE "ABCABCABC" TO WS-STRING.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-STRING TALLYING WS-TALLY
               FOR ALL "A".
           IF WS-TALLY = 3
               DISPLAY "NC115A-TEST-1 PASS"
           ELSE
               DISPLAY "NC115A-TEST-1 FAIL"
               DISPLAY "  Expected 3, got " WS-TALLY
           END-IF.
      * Test 2: INSPECT REPLACING ALL
           MOVE "HELLO WORLD" TO WS-STRING.
           INSPECT WS-STRING REPLACING ALL "L"
               BY "X".
           IF WS-STRING(1:11) = "HEXXO WORXD"
               DISPLAY "NC115A-TEST-2 PASS"
           ELSE
               DISPLAY "NC115A-TEST-2 FAIL"
               DISPLAY "  Expected HEXXO WORXD, got >"
                   WS-STRING "<"
           END-IF.
      * Test 3: INSPECT CONVERTING
           MOVE "ABCDEF" TO WS-STRING.
           INSPECT WS-STRING CONVERTING
               "ABCDEF" TO "abcdef".
           IF WS-STRING(1:6) = "abcdef"
               DISPLAY "NC115A-TEST-3 PASS"
           ELSE
               DISPLAY "NC115A-TEST-3 FAIL"
               DISPLAY "  Expected abcdef, got >"
                   WS-STRING "<"
           END-IF.
           STOP RUN.
