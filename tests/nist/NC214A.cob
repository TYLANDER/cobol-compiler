       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC214A.
      *
      * NIST CCVS-style test: MOVE between different PIC sizes
      * Tests truncation and padding when moving between fields
      * of different sizes (both alpha and numeric).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SHORT-ALPHA  PIC X(3)  VALUE SPACES.
       01 WS-LONG-ALPHA   PIC X(10) VALUE SPACES.
       01 WS-SRC-ALPHA    PIC X(7)  VALUE "ABCDEFG".
       01 WS-SHORT-NUM    PIC 9(3)  VALUE ZEROS.
       01 WS-LONG-NUM     PIC 9(7)  VALUE ZEROS.
       01 WS-SRC-NUM      PIC 9(5)  VALUE ZEROS.
       01 WS-TEMP         PIC 9(5)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: MOVE alpha to shorter field - truncation
      *   "ABCDEFG" (7 chars) to X(3) should give "ABC"
      *   Then move "AB" (short) to X(10) should pad with spaces
           MOVE WS-SRC-ALPHA TO WS-SHORT-ALPHA.
           MOVE "AB" TO WS-LONG-ALPHA.
           IF WS-SHORT-ALPHA = "ABC"
               AND WS-LONG-ALPHA = "AB        "
               DISPLAY "NC214A-TEST-1 PASS"
           ELSE
               DISPLAY "NC214A-TEST-1 FAIL"
               DISPLAY "  SHORT=>" WS-SHORT-ALPHA "<"
               DISPLAY "  LONG=>" WS-LONG-ALPHA "<"
           END-IF.
      * Test 2: MOVE numeric to shorter field - truncation
      *   12345 in PIC 9(5) moved to PIC 9(3) gives 345
      *   Then 42 in PIC 9(3) moved to PIC 9(7) gives 0000042
           MOVE 12345 TO WS-SRC-NUM.
           MOVE WS-SRC-NUM TO WS-SHORT-NUM.
           MOVE 42 TO WS-SHORT-NUM.
           MOVE WS-SHORT-NUM TO WS-LONG-NUM.
           IF WS-LONG-NUM = 42
               DISPLAY "NC214A-TEST-2 PASS"
           ELSE
               DISPLAY "NC214A-TEST-2 FAIL"
               DISPLAY "  LONG-NUM=" WS-LONG-NUM
           END-IF.
      * Test 3: MOVE numeric to larger field - zero padding
      *   99 in PIC 9(3) moved to PIC 9(7) gives 0000099
           MOVE 99 TO WS-SHORT-NUM.
           MOVE WS-SHORT-NUM TO WS-LONG-NUM.
           IF WS-LONG-NUM = 99
               DISPLAY "NC214A-TEST-3 PASS"
           ELSE
               DISPLAY "NC214A-TEST-3 FAIL"
               DISPLAY "  Expected 99, got " WS-LONG-NUM
           END-IF.
           STOP RUN.
