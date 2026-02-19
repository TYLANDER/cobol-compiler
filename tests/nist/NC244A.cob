       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC244A.
      *
      * NIST CCVS-style test: MOVE with reference modification
      * Tests extracting substrings using WS-A(start:length).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE   PIC X(20) VALUE "ABCDEFGHIJ".
       01 WS-DEST     PIC X(10) VALUE SPACES.
       01 WS-RESULT   PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Extract first 3 characters
      *   WS-SOURCE(1:3) should be "ABC"
           MOVE WS-SOURCE(1:3) TO WS-DEST.
           IF WS-DEST(1:3) = "ABC"
               DISPLAY "NC244A-TEST-1 PASS"
           ELSE
               DISPLAY "NC244A-TEST-1 FAIL"
               DISPLAY "  Expected ABC, got >"
                   WS-DEST "<"
           END-IF.
      * Test 2: Extract middle characters
      *   WS-SOURCE(4:3) should be "DEF"
           MOVE SPACES TO WS-DEST.
           MOVE WS-SOURCE(4:3) TO WS-DEST.
           IF WS-DEST(1:3) = "DEF"
               DISPLAY "NC244A-TEST-2 PASS"
           ELSE
               DISPLAY "NC244A-TEST-2 FAIL"
               DISPLAY "  Expected DEF, got >"
                   WS-DEST "<"
           END-IF.
      * Test 3: Move into a ref-modified target
      *   Place "XY" at position 5 of WS-RESULT
           MOVE "0123456789" TO WS-RESULT.
           MOVE "XY" TO WS-RESULT(5:2).
           IF WS-RESULT(1:4) = "0123"
               AND WS-RESULT(5:2) = "XY"
               AND WS-RESULT(7:1) = "6"
               DISPLAY "NC244A-TEST-3 PASS"
           ELSE
               DISPLAY "NC244A-TEST-3 FAIL"
               DISPLAY "  Expected 0123XY6789, got >"
                   WS-RESULT "<"
           END-IF.
           STOP RUN.
