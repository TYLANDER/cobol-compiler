       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC177A.
      *
      * NIST CCVS-style test: MOVE with different PIC sizes
      * Tests truncation (moving larger to smaller) and
      * padding (moving smaller to larger) for both
      * alphanumeric and numeric fields.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-LONG          PIC X(10) VALUE "ABCDEFGHIJ".
       01 WS-SHORT          PIC X(4)  VALUE SPACES.
       01 WS-WIDER          PIC X(8)  VALUE SPACES.
       01 WS-SRC3           PIC X(3)  VALUE "XYZ".
       01 WS-NUM-BIG        PIC 9(6)  VALUE 123456.
       01 WS-NUM-SMALL      PIC 9(3)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Alphanumeric truncation on the right
      *   "ABCDEFGHIJ" (10 chars) into PIC X(4) => "ABCD"
           MOVE WS-LONG TO WS-SHORT.
           IF WS-SHORT = "ABCD"
               DISPLAY "NC177A-TEST-1 PASS"
           ELSE
               DISPLAY "NC177A-TEST-1 FAIL"
               DISPLAY "  Expected [ABCD] got ["
                   WS-SHORT "]"
           END-IF.
      * Test 2: Alphanumeric padding with spaces on right
      *   "XYZ" (3 chars) into PIC X(8) => "XYZ     "
           MOVE WS-SRC3 TO WS-WIDER.
           IF WS-WIDER = "XYZ     "
               DISPLAY "NC177A-TEST-2 PASS"
           ELSE
               DISPLAY "NC177A-TEST-2 FAIL"
               DISPLAY "  Expected [XYZ     ] got ["
                   WS-WIDER "]"
           END-IF.
      * Test 3: Numeric truncation on the left
      *   123456 into PIC 9(3) => 456 (left truncated)
           MOVE WS-NUM-BIG TO WS-NUM-SMALL.
           IF WS-NUM-SMALL = 456
               DISPLAY "NC177A-TEST-3 PASS"
           ELSE
               DISPLAY "NC177A-TEST-3 FAIL"
               DISPLAY "  Expected 456 got "
                   WS-NUM-SMALL
           END-IF.
           STOP RUN.
