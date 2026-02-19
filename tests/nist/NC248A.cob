       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC248A.
      *
      * NIST CCVS-style test: Numeric comparison of different-sized
      * PIC fields. Tests that numeric comparisons work correctly
      * regardless of field size.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SMALL    PIC 9(2) VALUE ZEROS.
       01 WS-MEDIUM   PIC 9(5) VALUE ZEROS.
       01 WS-LARGE    PIC 9(8) VALUE ZEROS.
       01 WS-FLAG     PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Compare PIC 9(2) with PIC 9(5)
      *   Both hold value 42, should be equal
           MOVE 42 TO WS-SMALL.
           MOVE 42 TO WS-MEDIUM.
           IF WS-SMALL = WS-MEDIUM
               DISPLAY "NC248A-TEST-1 PASS"
           ELSE
               DISPLAY "NC248A-TEST-1 FAIL"
               DISPLAY "  " WS-SMALL " should equal "
                   WS-MEDIUM
           END-IF.
      * Test 2: Compare PIC 9(2) with PIC 9(8)
      *   Small=99, Large=100 => Small < Large
           MOVE 99 TO WS-SMALL.
           MOVE 100 TO WS-LARGE.
           MOVE 0 TO WS-FLAG.
           IF WS-SMALL < WS-LARGE
               MOVE 1 TO WS-FLAG
           END-IF.
           IF WS-FLAG = 1
               DISPLAY "NC248A-TEST-2 PASS"
           ELSE
               DISPLAY "NC248A-TEST-2 FAIL"
               DISPLAY "  99 should be less than 100"
           END-IF.
      * Test 3: Compare PIC 9(5) with literal
      *   Medium=10000, compare with literal 10000
           MOVE 10000 TO WS-MEDIUM.
           IF WS-MEDIUM = 10000
               DISPLAY "NC248A-TEST-3 PASS"
           ELSE
               DISPLAY "NC248A-TEST-3 FAIL"
               DISPLAY "  Expected 10000, got " WS-MEDIUM
           END-IF.
           STOP RUN.
