       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC301A.
      *
      * NIST CCVS-style test: MOVE between different numeric PIC sizes
      * Tests moving small PIC to large PIC (zero-fill) and large
      * PIC to small PIC (truncation of high-order digits).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SMALL        PIC 9(2) VALUE ZEROS.
       01 WS-LARGE        PIC 9(5) VALUE ZEROS.
       01 WS-MED          PIC 9(3) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: MOVE small PIC 9(2) to large PIC 9(5)
      *   42 in PIC 9(2) moved to PIC 9(5) => 00042
           MOVE 42 TO WS-SMALL.
           MOVE WS-SMALL TO WS-LARGE.
           IF WS-LARGE = 42
               DISPLAY "NC301A-TEST-1 PASS"
           ELSE
               DISPLAY "NC301A-TEST-1 FAIL"
               DISPLAY "  Expected 00042, got " WS-LARGE
           END-IF.
      * Test 2: MOVE large PIC 9(5) to small PIC 9(2)
      *   12345 moved to PIC 9(2) => truncated to 45
           MOVE 12345 TO WS-LARGE.
           MOVE WS-LARGE TO WS-SMALL.
           IF WS-SMALL = 45
               DISPLAY "NC301A-TEST-2 PASS"
           ELSE
               DISPLAY "NC301A-TEST-2 FAIL"
               DISPLAY "  Expected 45, got " WS-SMALL
           END-IF.
      * Test 3: MOVE medium PIC 9(3) to small PIC 9(2)
      *   789 moved to PIC 9(2) => truncated to 89
           MOVE 789 TO WS-MED.
           MOVE WS-MED TO WS-SMALL.
           IF WS-SMALL = 89
               DISPLAY "NC301A-TEST-3 PASS"
           ELSE
               DISPLAY "NC301A-TEST-3 FAIL"
               DISPLAY "  Expected 89, got " WS-SMALL
           END-IF.
           STOP RUN.
