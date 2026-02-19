       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC249A.
      *
      * NIST CCVS-style test: INITIALIZE statement
      * Tests default INITIALIZE behavior (without REPLACING):
      * alphanumeric fields get SPACES, numeric fields get ZEROS.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP-1.
           05 WS-G1-NAME  PIC X(8)  VALUE "TESTNAME".
           05 WS-G1-CODE  PIC X(3)  VALUE "XYZ".
           05 WS-G1-AMT   PIC 9(5)  VALUE 99999.
           05 WS-G1-QTY   PIC 9(3)  VALUE 888.
       01 WS-ALPHA-ELEM   PIC X(10) VALUE "ABCDEFGHIJ".
       01 WS-NUM-ELEM     PIC 9(6)  VALUE 123456.
       PROCEDURE DIVISION.
      * Test 1: INITIALIZE group item
      *   All alpha fields become SPACES, all numeric become 0
           INITIALIZE WS-GROUP-1.
           IF WS-G1-NAME = SPACES
               AND WS-G1-CODE = SPACES
               AND WS-G1-AMT = 0
               AND WS-G1-QTY = 0
               DISPLAY "NC249A-TEST-1 PASS"
           ELSE
               DISPLAY "NC249A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-G1-NAME "<"
               DISPLAY "  CODE=>" WS-G1-CODE "<"
               DISPLAY "  AMT=" WS-G1-AMT
               DISPLAY "  QTY=" WS-G1-QTY
           END-IF.
      * Test 2: INITIALIZE elementary alphanumeric
      *   Should become all SPACES
           MOVE "ABCDEFGHIJ" TO WS-ALPHA-ELEM.
           INITIALIZE WS-ALPHA-ELEM.
           IF WS-ALPHA-ELEM = SPACES
               DISPLAY "NC249A-TEST-2 PASS"
           ELSE
               DISPLAY "NC249A-TEST-2 FAIL"
               DISPLAY "  Expected SPACES, got >"
                   WS-ALPHA-ELEM "<"
           END-IF.
      * Test 3: INITIALIZE elementary numeric
      *   Should become 0
           MOVE 123456 TO WS-NUM-ELEM.
           INITIALIZE WS-NUM-ELEM.
           IF WS-NUM-ELEM = 0
               DISPLAY "NC249A-TEST-3 PASS"
           ELSE
               DISPLAY "NC249A-TEST-3 FAIL"
               DISPLAY "  Expected 0, got " WS-NUM-ELEM
           END-IF.
           STOP RUN.
