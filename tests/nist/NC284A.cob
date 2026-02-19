       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC284A.
      *
      * NIST CCVS-style test: MOVE with reference modification
      * Tests substring extraction using reference modification
      * on the source field of a MOVE statement.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA         PIC X(20) VALUE "ABCDEFGHIJKLMNOPQRST".
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       01 WS-POS          PIC 9(4)  VALUE ZEROS.
       01 WS-LEN          PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Extract first 5 characters using ref mod
      *   "ABCDEFGHIJKLMNOPQRST"(1:5) => "ABCDE"
           MOVE SPACES TO WS-RESULT.
           MOVE WS-DATA(1:5) TO WS-RESULT.
           IF WS-RESULT(1:5) = "ABCDE"
               DISPLAY "NC284A-TEST-1 PASS"
           ELSE
               DISPLAY "NC284A-TEST-1 FAIL"
               DISPLAY "  Expected ABCDE, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 2: Extract from middle using computed position
      *   Position 8, length 4 => "HIJK"
           MOVE SPACES TO WS-RESULT.
           MOVE 8 TO WS-POS.
           MOVE 4 TO WS-LEN.
           MOVE WS-DATA(WS-POS:WS-LEN) TO WS-RESULT.
           IF WS-RESULT(1:4) = "HIJK"
               DISPLAY "NC284A-TEST-2 PASS"
           ELSE
               DISPLAY "NC284A-TEST-2 FAIL"
               DISPLAY "  Expected HIJK, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 3: Reference modification on destination
      *   Write "XYZ" into positions 3-5 of result
           MOVE "AAAAAAAAAA" TO WS-RESULT.
           MOVE "XYZ" TO WS-RESULT(3:3).
           IF WS-RESULT(1:2) = "AA"
               AND WS-RESULT(3:3) = "XYZ"
               AND WS-RESULT(6:5) = "AAAAA"
               DISPLAY "NC284A-TEST-3 PASS"
           ELSE
               DISPLAY "NC284A-TEST-3 FAIL"
               DISPLAY "  Expected AAXYZAAAAA, got >"
                   WS-RESULT "<"
           END-IF.
           STOP RUN.
