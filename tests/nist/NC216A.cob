       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC216A.
      *
      * NIST CCVS-style test: Multiple level data items
      * Tests 01, 05, 10, 15 levels with FILLER, verifying
      * group item structure and subordinate access.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RECORD.
           05 WS-NAME.
               10 WS-FIRST    PIC X(5) VALUE "JOHN ".
               10 FILLER      PIC X(1) VALUE " ".
               10 WS-LAST     PIC X(5) VALUE "SMITH".
           05 WS-ADDRESS.
               10 WS-STREET   PIC X(10) VALUE "123 MAIN  ".
               10 WS-CITY.
                   15 WS-CNAME PIC X(6) VALUE "BOSTON".
                   15 FILLER   PIC X(1) VALUE " ".
                   15 WS-STATE PIC X(2) VALUE "MA".
       01 WS-FULL-NAME    PIC X(11) VALUE SPACES.
       01 WS-CITY-STATE   PIC X(9)  VALUE SPACES.
       01 WS-REC-LEN      PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Access subordinate items and group items
      *   WS-NAME should be "JOHN  SMITH" (11 chars)
           MOVE WS-NAME TO WS-FULL-NAME.
           IF WS-FULL-NAME = "JOHN  SMITH"
               DISPLAY "NC216A-TEST-1 PASS"
           ELSE
               DISPLAY "NC216A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-FULL-NAME "<"
           END-IF.
      * Test 2: Access deeply nested items
      *   WS-CNAME should be "BOSTON", WS-STATE should be "MA"
      *   WS-CITY group should be "BOSTON MA"
           MOVE WS-CITY TO WS-CITY-STATE.
           IF WS-CNAME = "BOSTON"
               AND WS-STATE = "MA"
               AND WS-CITY-STATE = "BOSTON MA"
               DISPLAY "NC216A-TEST-2 PASS"
           ELSE
               DISPLAY "NC216A-TEST-2 FAIL"
               DISPLAY "  CNAME=>" WS-CNAME "<"
               DISPLAY "  STATE=>" WS-STATE "<"
               DISPLAY "  CITY=>" WS-CITY-STATE "<"
           END-IF.
      * Test 3: Modify subordinate and check group
      *   Change WS-FIRST to "JANE " and verify WS-NAME
      *   WS-NAME = "JANE " + " " + "SMITH" = "JANE  SMITH"
           MOVE "JANE " TO WS-FIRST.
           MOVE WS-NAME TO WS-FULL-NAME.
           IF WS-FIRST = "JANE "
               AND WS-LAST = "SMITH"
               AND WS-FULL-NAME = "JANE  SMITH"
               DISPLAY "NC216A-TEST-3 PASS"
           ELSE
               DISPLAY "NC216A-TEST-3 FAIL"
               DISPLAY "  FIRST=>" WS-FIRST "<"
               DISPLAY "  LAST=>" WS-LAST "<"
               DISPLAY "  NAME=>" WS-FULL-NAME "<"
           END-IF.
           STOP RUN.
