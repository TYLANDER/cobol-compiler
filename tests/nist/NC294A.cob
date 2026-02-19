       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC294A.
      *
      * NIST CCVS-style test: JUSTIFIED RIGHT clause
      * Tests MOVE of alphanumeric data into fields declared
      * with JUSTIFIED RIGHT (data is right-aligned, padded
      * with spaces on the left).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SHORT        PIC X(3) VALUE "HI ".
       01 WS-LONG         PIC X(10) VALUE "ABCDEFGHIJ".
       01 WS-DEST1        PIC X(8) JUSTIFIED RIGHT.
       01 WS-DEST2        PIC X(4) JUST RIGHT.
       01 WS-DEST3        PIC X(6) JUST RIGHT.
       PROCEDURE DIVISION.
      * Test 1: Short string into JUSTIFIED RIGHT field
      *   "HI " (3 chars) into PIC X(8) JUST RIGHT => "     HI "
           MOVE SPACES TO WS-DEST1.
           MOVE WS-SHORT TO WS-DEST1.
           IF WS-DEST1 = "     HI "
               DISPLAY "NC294A-TEST-1 PASS"
           ELSE
               DISPLAY "NC294A-TEST-1 FAIL"
               DISPLAY "  Expected [     HI ] got ["
                   WS-DEST1 "]"
           END-IF.
      * Test 2: Long string truncated on left for JUST RIGHT
      *   "ABCDEFGHIJ" (10 chars) into PIC X(4) JUST RIGHT
      *   => truncated from left => "GHIJ"
           MOVE SPACES TO WS-DEST2.
           MOVE WS-LONG TO WS-DEST2.
           IF WS-DEST2 = "GHIJ"
               DISPLAY "NC294A-TEST-2 PASS"
           ELSE
               DISPLAY "NC294A-TEST-2 FAIL"
               DISPLAY "  Expected [GHIJ] got ["
                   WS-DEST2 "]"
           END-IF.
      * Test 3: Literal into JUST RIGHT field
      *   "AB" (2 chars) into PIC X(6) JUST RIGHT => "    AB"
           MOVE SPACES TO WS-DEST3.
           MOVE "AB" TO WS-DEST3.
           IF WS-DEST3 = "    AB"
               DISPLAY "NC294A-TEST-3 PASS"
           ELSE
               DISPLAY "NC294A-TEST-3 FAIL"
               DISPLAY "  Expected [    AB] got ["
                   WS-DEST3 "]"
           END-IF.
           STOP RUN.
