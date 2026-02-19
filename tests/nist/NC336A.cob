       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC336A.
      *
      * NIST CCVS-style test: Numeric moves with truncation
      * and zero-padding.
      * Tests that moving between different PIC 9 sizes correctly
      * truncates high-order digits or pads with leading zeros.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC5          PIC 9(5) VALUE 0.
       01 WS-DST3          PIC 9(3) VALUE 0.
       01 WS-SRC2          PIC 9(2) VALUE 0.
       01 WS-DST6          PIC 9(6) VALUE 0.
       01 WS-FILLER1       PIC X(1) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Move PIC 9(5) value 67890 to PIC 9(3)
      *   High-order digits truncated: 67890 => 890
           MOVE 67890 TO WS-SRC5.
           MOVE WS-SRC5 TO WS-DST3.
           IF WS-DST3 = 890
               DISPLAY "NC336A-TEST-1 PASS"
           ELSE
               DISPLAY "NC336A-TEST-1 FAIL"
               DISPLAY "  Expected 890, got " WS-DST3
           END-IF.
      * Test 2: Move PIC 9(2) value 42 to PIC 9(6)
      *   Zero-padded on the left: 42 => 000042
           MOVE 42 TO WS-SRC2.
           MOVE WS-SRC2 TO WS-DST6.
           IF WS-DST6 = 42
               DISPLAY "NC336A-TEST-2 PASS"
           ELSE
               DISPLAY "NC336A-TEST-2 FAIL"
               DISPLAY "  Expected 000042, got " WS-DST6
           END-IF.
      * Test 3: Move literal 5 to PIC 9(5) then to PIC 9(3)
      *   00005 moved to PIC 9(3) => 005 which is 5
           MOVE 5 TO WS-SRC5.
           MOVE WS-SRC5 TO WS-DST3.
           IF WS-DST3 = 5
               DISPLAY "NC336A-TEST-3 PASS"
           ELSE
               DISPLAY "NC336A-TEST-3 FAIL"
               DISPLAY "  Expected 5, got " WS-DST3
           END-IF.
           STOP RUN.
