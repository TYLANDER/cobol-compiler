       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC295A.
      *
      * NIST CCVS-style test: USAGE COMP
      * Tests COMP (binary) numeric fields with small values
      * to verify arithmetic and comparison work correctly.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COMP1       PIC 9(4) USAGE COMP VALUE 5.
       01 WS-COMP2       PIC 9(4) USAGE COMP VALUE 3.
       01 WS-COMP3       PIC 9(4) USAGE COMP VALUE ZEROS.
       01 WS-DISP        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: ADD two small COMP fields
      *   5 + 3 = 8
           MOVE 5 TO WS-COMP1.
           MOVE 3 TO WS-COMP2.
           ADD WS-COMP1 WS-COMP2 GIVING WS-COMP3.
           IF WS-COMP3 = 8
               DISPLAY "NC295A-TEST-1 PASS"
           ELSE
               DISPLAY "NC295A-TEST-1 FAIL"
               DISPLAY "  Expected 8, got " WS-COMP3
           END-IF.
      * Test 2: MULTIPLY small COMP value
      *   7 * 6 = 42
           MOVE 7 TO WS-COMP1.
           MULTIPLY 6 BY WS-COMP1.
           IF WS-COMP1 = 42
               DISPLAY "NC295A-TEST-2 PASS"
           ELSE
               DISPLAY "NC295A-TEST-2 FAIL"
               DISPLAY "  Expected 42, got " WS-COMP1
           END-IF.
      * Test 3: SUBTRACT with COMP fields
      *   50 - 15 = 35
           MOVE 50 TO WS-COMP1.
           MOVE 15 TO WS-COMP2.
           SUBTRACT WS-COMP2 FROM WS-COMP1.
           IF WS-COMP1 = 35
               DISPLAY "NC295A-TEST-3 PASS"
           ELSE
               DISPLAY "NC295A-TEST-3 FAIL"
               DISPLAY "  Expected 35, got " WS-COMP1
           END-IF.
           STOP RUN.
