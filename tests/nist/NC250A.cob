       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC250A.
      *
      * NIST CCVS-style test: INSPECT REPLACING LEADING
      * Tests INSPECT REPLACING LEADING to modify leading
      * occurrences of a character.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA     PIC X(20) VALUE SPACES.
       01 WS-DATA2    PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Replace leading zeros with spaces
      *   "00012300" => "   12300"
           MOVE "00012300" TO WS-DATA.
           INSPECT WS-DATA REPLACING LEADING "0" BY " ".
           IF WS-DATA(1:8) = "   12300"
               DISPLAY "NC250A-TEST-1 PASS"
           ELSE
               DISPLAY "NC250A-TEST-1 FAIL"
               DISPLAY "  Expected >   12300<, got >"
                   WS-DATA(1:8) "<"
           END-IF.
      * Test 2: Replace leading spaces with asterisks
      *   "   HELLO" => "***HELLO"
           MOVE "   HELLO" TO WS-DATA.
           INSPECT WS-DATA REPLACING LEADING " " BY "*".
           IF WS-DATA(1:8) = "***HELLO"
               DISPLAY "NC250A-TEST-2 PASS"
           ELSE
               DISPLAY "NC250A-TEST-2 FAIL"
               DISPLAY "  Expected ***HELLO, got >"
                   WS-DATA(1:8) "<"
           END-IF.
      * Test 3: LEADING with no matching prefix
      *   "ABCAAA" has no leading "X", so no change
           MOVE "ABCAAA" TO WS-DATA2.
           INSPECT WS-DATA2 REPLACING LEADING "X" BY "Z".
           IF WS-DATA2(1:6) = "ABCAAA"
               DISPLAY "NC250A-TEST-3 PASS"
           ELSE
               DISPLAY "NC250A-TEST-3 FAIL"
               DISPLAY "  Expected ABCAAA, got >"
                   WS-DATA2(1:6) "<"
           END-IF.
           STOP RUN.
