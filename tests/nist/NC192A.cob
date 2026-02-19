       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC192A.
      *
      * NIST CCVS-style test: MOVE between different numeric scales
      * Tests MOVE of numeric values between fields with different
      * PIC sizes and decimal positions to verify truncation and
      * zero-fill behavior.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC-3V2     PIC 9(3)V99  VALUE 123.45.
       01 WS-DST-5V1     PIC 9(5)V9   VALUE ZEROS.
       01 WS-SRC-2V3     PIC 9(2)V999 VALUE 12.345.
       01 WS-DST-4V0     PIC 9(4)     VALUE ZEROS.
       01 WS-SRC-5V0     PIC 9(5)     VALUE 54321.
       01 WS-DST-3V2     PIC 9(3)V99  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: MOVE PIC 9(3)V99 to PIC 9(5)V9
      *   123.45 -> should become 00123.4 (truncate low-order decimal)
           MOVE 123.45 TO WS-SRC-3V2.
           MOVE ZEROS TO WS-DST-5V1.
           MOVE WS-SRC-3V2 TO WS-DST-5V1.
           IF WS-DST-5V1 = 123.4
               DISPLAY "NC192A-TEST-1 PASS"
           ELSE
               DISPLAY "NC192A-TEST-1 FAIL"
               DISPLAY "  Expected 123.4, got " WS-DST-5V1
           END-IF.
      * Test 2: MOVE PIC 9(2)V999 to PIC 9(4) (integer only)
      *   12.345 -> should become 0012 (decimal portion truncated)
           MOVE 12.345 TO WS-SRC-2V3.
           MOVE ZEROS TO WS-DST-4V0.
           MOVE WS-SRC-2V3 TO WS-DST-4V0.
           IF WS-DST-4V0 = 12
               DISPLAY "NC192A-TEST-2 PASS"
           ELSE
               DISPLAY "NC192A-TEST-2 FAIL"
               DISPLAY "  Expected 12, got " WS-DST-4V0
           END-IF.
      * Test 3: MOVE PIC 9(5) to PIC 9(3)V99
      *   54321 -> should become 321.00 (high-order truncation)
           MOVE 54321 TO WS-SRC-5V0.
           MOVE ZEROS TO WS-DST-3V2.
           MOVE WS-SRC-5V0 TO WS-DST-3V2.
           IF WS-DST-3V2 = 321.00
               DISPLAY "NC192A-TEST-3 PASS"
           ELSE
               DISPLAY "NC192A-TEST-3 FAIL"
               DISPLAY "  Expected 321.00, got " WS-DST-3V2
           END-IF.
           STOP RUN.
