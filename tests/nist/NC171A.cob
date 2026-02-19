       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC171A.
      *
      * NIST CCVS-style test: JUSTIFIED RIGHT clause
      * Tests MOVE of alphanumeric data into a field declared
      * with JUSTIFIED RIGHT (JUST RIGHT).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE1       PIC X(3)  VALUE "ABC".
       01 WS-SOURCE2       PIC X(10) VALUE "LONGSTRING".
       01 WS-DEST1         PIC X(6)  JUSTIFIED RIGHT.
       01 WS-DEST2         PIC X(5)  JUST RIGHT.
       PROCEDURE DIVISION.
      * Test 1: Short string moved to JUSTIFIED RIGHT field
      *   "ABC" into PIC X(6) JUST RIGHT => "   ABC"
           MOVE SPACES TO WS-DEST1.
           MOVE WS-SOURCE1 TO WS-DEST1.
           IF WS-DEST1 = "   ABC"
               DISPLAY "NC171A-TEST-1 PASS"
           ELSE
               DISPLAY "NC171A-TEST-1 FAIL"
               DISPLAY "  Expected [   ABC] got ["
                   WS-DEST1 "]"
           END-IF.
      * Test 2: Long string truncated on left for JUST RIGHT
      *   "LONGSTRING" (10 chars) into PIC X(5) JUST RIGHT
      *   => right-justified, truncated from LEFT => "TRING"
           MOVE SPACES TO WS-DEST2.
           MOVE WS-SOURCE2 TO WS-DEST2.
           IF WS-DEST2 = "TRING"
               DISPLAY "NC171A-TEST-2 PASS"
           ELSE
               DISPLAY "NC171A-TEST-2 FAIL"
               DISPLAY "  Expected [TRING] got ["
                   WS-DEST2 "]"
           END-IF.
           STOP RUN.
