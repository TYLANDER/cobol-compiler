       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC161A.
      *
      * NIST CCVS-style test: MOVE CORRESPONDING between groups
      * Tests MOVE CORRESPONDING to move only fields with matching
      * names between two group items.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP-SRC.
           05 WS-NAME       PIC X(10) VALUE "JOHN".
           05 WS-AGE        PIC 9(3)  VALUE 30.
           05 WS-CITY       PIC X(10) VALUE "BOSTON".
       01 WS-GROUP-DST.
           05 WS-NAME       PIC X(10) VALUE SPACES.
           05 WS-AGE        PIC 9(3)  VALUE ZEROS.
           05 WS-STATE      PIC X(2)  VALUE SPACES.
       01 WS-GRP-A.
           05 WS-X          PIC 9(4) VALUE 100.
           05 WS-Y          PIC 9(4) VALUE 200.
           05 WS-Z          PIC 9(4) VALUE 300.
       01 WS-GRP-B.
           05 WS-X          PIC 9(4) VALUE ZEROS.
           05 WS-Y          PIC 9(4) VALUE ZEROS.
           05 WS-W          PIC 9(4) VALUE 999.
       PROCEDURE DIVISION.
      * Test 1: MOVE CORRESPONDING copies matching fields only
      *   WS-NAME and WS-AGE exist in both groups, WS-CITY does not
      *   WS-STATE in DST should remain SPACES
           MOVE CORRESPONDING WS-GROUP-SRC TO WS-GROUP-DST.
           IF WS-NAME OF WS-GROUP-DST = "JOHN"
               AND WS-AGE OF WS-GROUP-DST = 30
               AND WS-STATE OF WS-GROUP-DST = SPACES
               DISPLAY "NC161A-TEST-1 PASS"
           ELSE
               DISPLAY "NC161A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-NAME OF WS-GROUP-DST "<"
               DISPLAY "  AGE=" WS-AGE OF WS-GROUP-DST
               DISPLAY "  STATE=>" WS-STATE OF WS-GROUP-DST "<"
           END-IF.
      * Test 2: MOVE CORR with numeric fields
      *   WS-X and WS-Y match; WS-Z and WS-W do not
      *   WS-W should remain 999
           MOVE CORRESPONDING WS-GRP-A TO WS-GRP-B.
           IF WS-X OF WS-GRP-B = 100
               AND WS-Y OF WS-GRP-B = 200
               AND WS-W OF WS-GRP-B = 999
               DISPLAY "NC161A-TEST-2 PASS"
           ELSE
               DISPLAY "NC161A-TEST-2 FAIL"
               DISPLAY "  X=" WS-X OF WS-GRP-B
               DISPLAY "  Y=" WS-Y OF WS-GRP-B
               DISPLAY "  W=" WS-W OF WS-GRP-B
           END-IF.
           STOP RUN.
