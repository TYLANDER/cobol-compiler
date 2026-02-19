       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC325A.
      *
      * NIST CCVS-style test: Reference modification
      * Tests reference modification with variable positions.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA         PIC X(20) VALUE "ABCDEFGHIJKLMNOPQRST".
       01 WS-POS           PIC 9(2) VALUE 0.
       01 WS-RESULT        PIC X(5) VALUE SPACES.
       01 WS-RESULT3       PIC X(3) VALUE SPACES.
       01 WS-CHAR          PIC X(1) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Reference modification with variable position
      *   Extract 5 characters starting at position 3 => "CDEFG"
           MOVE SPACES TO WS-RESULT.
           MOVE 3 TO WS-POS.
           MOVE WS-DATA(WS-POS:5) TO WS-RESULT.
           IF WS-RESULT = "CDEFG"
               DISPLAY "NC325A-TEST-1 PASS"
           ELSE
               DISPLAY "NC325A-TEST-1 FAIL"
               DISPLAY "  Expected 'CDEFG', got '" WS-RESULT
                   "'"
           END-IF.
      * Test 2: Reference modification at end of string
      *   Extract 3 characters starting at position 18 => "RST"
           MOVE SPACES TO WS-RESULT3.
           MOVE 18 TO WS-POS.
           MOVE WS-DATA(WS-POS:3) TO WS-RESULT3.
           IF WS-RESULT3 = "RST"
               DISPLAY "NC325A-TEST-2 PASS"
           ELSE
               DISPLAY "NC325A-TEST-2 FAIL"
               DISPLAY "  Expected 'RST', got '" WS-RESULT3
                   "'"
           END-IF.
      * Test 3: Single character extraction using ref-mod
      *   Extract 1 character at position 10 => "J"
           MOVE SPACES TO WS-CHAR.
           MOVE 10 TO WS-POS.
           MOVE WS-DATA(WS-POS:1) TO WS-CHAR.
           IF WS-CHAR = "J"
               DISPLAY "NC325A-TEST-3 PASS"
           ELSE
               DISPLAY "NC325A-TEST-3 FAIL"
               DISPLAY "  Expected 'J', got '" WS-CHAR "'"
           END-IF.
           STOP RUN.
