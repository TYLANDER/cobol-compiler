       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC326A.
      *
      * NIST CCVS-style test: INSPECT REPLACING patterns
      * Tests INSPECT REPLACING LEADING and FIRST.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA1        PIC X(10) VALUE SPACES.
       01 WS-DATA2        PIC X(10) VALUE SPACES.
       01 WS-DATA3        PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: INSPECT REPLACING ALL
      *   Replace all "A" with "X" in "ABACADAEAF"
           MOVE "ABACANAEAF" TO WS-DATA1.
           INSPECT WS-DATA1 REPLACING ALL "A" BY "X".
           IF WS-DATA1 = "XBXCXNXEXF"
               DISPLAY "NC326A-TEST-1 PASS"
           ELSE
               DISPLAY "NC326A-TEST-1 FAIL"
               DISPLAY "  Expected 'XBXCXNXEXF', got '"
                   WS-DATA1 "'"
           END-IF.
      * Test 2: INSPECT REPLACING LEADING
      *   Replace leading zeros with spaces in "000123    "
           MOVE "000123    " TO WS-DATA2.
           INSPECT WS-DATA2 REPLACING LEADING "0" BY " ".
           IF WS-DATA2 = "   123    "
               DISPLAY "NC326A-TEST-2 PASS"
           ELSE
               DISPLAY "NC326A-TEST-2 FAIL"
               DISPLAY "  Expected '   123    ', got '"
                   WS-DATA2 "'"
           END-IF.
      * Test 3: INSPECT REPLACING FIRST
      *   Replace first "B" with "Z" in "ABCBDBFB  "
           MOVE "ABCBDBFB  " TO WS-DATA3.
           INSPECT WS-DATA3 REPLACING FIRST "B" BY "Z".
           IF WS-DATA3 = "AZCBDBFB  "
               DISPLAY "NC326A-TEST-3 PASS"
           ELSE
               DISPLAY "NC326A-TEST-3 FAIL"
               DISPLAY "  Expected 'AZCBDBFB  ', got '"
                   WS-DATA3 "'"
           END-IF.
           STOP RUN.
