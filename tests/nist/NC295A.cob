       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC295A.
      *
      * NIST CCVS-style test: Arithmetic with signed fields.
      * Tests ADD/MULTIPLY/SUBTRACT with PIC S9 DISPLAY fields
      * to verify arithmetic works correctly with signed values.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC S9(4) VALUE 5.
       01 WS-B        PIC S9(4) VALUE 3.
       01 WS-C        PIC S9(4) VALUE ZEROS.
       01 WS-DISP     PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: ADD two signed fields
      *   5 + 3 = 8
           MOVE 5 TO WS-A.
           MOVE 3 TO WS-B.
           ADD WS-A WS-B GIVING WS-C.
           IF WS-C = 8
               DISPLAY "NC295A-TEST-1 PASS"
           ELSE
               DISPLAY "NC295A-TEST-1 FAIL"
               DISPLAY "  Expected 8, got " WS-C
           END-IF.
      * Test 2: MULTIPLY signed value
      *   7 * 6 = 42
           MOVE 7 TO WS-A.
           MULTIPLY 6 BY WS-A.
           IF WS-A = 42
               DISPLAY "NC295A-TEST-2 PASS"
           ELSE
               DISPLAY "NC295A-TEST-2 FAIL"
               DISPLAY "  Expected 42, got " WS-A
           END-IF.
      * Test 3: SUBTRACT signed fields
      *   50 - 15 = 35
           MOVE 50 TO WS-A.
           MOVE 15 TO WS-B.
           SUBTRACT WS-B FROM WS-A.
           IF WS-A = 35
               DISPLAY "NC295A-TEST-3 PASS"
           ELSE
               DISPLAY "NC295A-TEST-3 FAIL"
               DISPLAY "  Expected 35, got " WS-A
           END-IF.
           STOP RUN.
