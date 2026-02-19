       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC238A.
      *
      * NIST CCVS-style test: INSPECT TALLYING with
      * ALL, LEADING, and BEFORE INITIAL options.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA     PIC X(20) VALUE SPACES.
       01 WS-TALLY    PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: INSPECT TALLYING ALL "A" in a string
      *   "ABRACADABRA" has 5 A's
           MOVE "ABRACADABRA" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR ALL "A".
           IF WS-TALLY = 5
               DISPLAY "NC238A-TEST-1 PASS"
           ELSE
               DISPLAY "NC238A-TEST-1 FAIL"
               DISPLAY "  Expected 5, got " WS-TALLY
           END-IF.
      * Test 2: INSPECT TALLYING LEADING spaces
      *   "   XYZ" has 3 leading spaces
           MOVE "   XYZ" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR LEADING SPACES.
           IF WS-TALLY = 3
               DISPLAY "NC238A-TEST-2 PASS"
           ELSE
               DISPLAY "NC238A-TEST-2 FAIL"
               DISPLAY "  Expected 3, got " WS-TALLY
           END-IF.
      * Test 3: INSPECT TALLYING with BEFORE INITIAL
      *   "HELLO WORLD" tally ALL "L" BEFORE INITIAL " "
      *   Only counts L's before the space: "HELLO" has 2 L's
           MOVE "HELLO WORLD" TO WS-DATA.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA TALLYING WS-TALLY
               FOR ALL "L" BEFORE INITIAL " ".
           IF WS-TALLY = 2
               DISPLAY "NC238A-TEST-3 PASS"
           ELSE
               DISPLAY "NC238A-TEST-3 FAIL"
               DISPLAY "  Expected 2, got " WS-TALLY
           END-IF.
           STOP RUN.
