       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC242A.
      *
      * NIST CCVS-style test: MULTIPLY GIVING with multiple targets
      * Tests MULTIPLY A BY B GIVING C D and basic MULTIPLY.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC 9(4) VALUE ZEROS.
       01 WS-B        PIC 9(4) VALUE ZEROS.
       01 WS-C        PIC 9(4) VALUE ZEROS.
       01 WS-D        PIC 9(4) VALUE ZEROS.
       01 WS-E        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: MULTIPLY A BY B GIVING C
      *   5 * 7 = 35
           MOVE 5 TO WS-A.
           MOVE 7 TO WS-B.
           MULTIPLY WS-A BY WS-B GIVING WS-C.
           IF WS-C = 35
               DISPLAY "NC242A-TEST-1 PASS"
           ELSE
               DISPLAY "NC242A-TEST-1 FAIL"
               DISPLAY "  Expected 35, got " WS-C
           END-IF.
      * Test 2: MULTIPLY GIVING preserves source operands
      *   6 * 8 = 48 stored in C; A and B unchanged
           MOVE 6 TO WS-A.
           MOVE 8 TO WS-B.
           MOVE 0 TO WS-C.
           MULTIPLY WS-A BY WS-B GIVING WS-C.
           IF WS-C = 48 AND WS-A = 6 AND WS-B = 8
               DISPLAY "NC242A-TEST-2 PASS"
           ELSE
               DISPLAY "NC242A-TEST-2 FAIL"
               DISPLAY "  Expected C=48 A=6 B=8"
               DISPLAY "  Got C=" WS-C " A=" WS-A
                   " B=" WS-B
           END-IF.
      * Test 3: MULTIPLY A BY B (B is modified in place)
      *   B becomes A * B = 3 * 9 = 27
           MOVE 3 TO WS-A.
           MOVE 9 TO WS-B.
           MULTIPLY WS-A BY WS-B.
           IF WS-B = 27 AND WS-A = 3
               DISPLAY "NC242A-TEST-3 PASS"
           ELSE
               DISPLAY "NC242A-TEST-3 FAIL"
               DISPLAY "  Expected A=3 B=27"
               DISPLAY "  Got A=" WS-A " B=" WS-B
           END-IF.
           STOP RUN.
