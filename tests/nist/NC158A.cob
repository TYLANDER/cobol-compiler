       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC158A.
      *
      * NIST CCVS-style test: Reference modification with variables
      * Tests reference modification using variable start positions
      * and lengths, i.e., field(expr:expr) syntax.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA          PIC X(20) VALUE "ABCDEFGHIJKLMNOPQRST".
       01 WS-START          PIC 9(4) VALUE ZEROS.
       01 WS-LEN            PIC 9(4) VALUE ZEROS.
       01 WS-RESULT         PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Variable start position
      *   Extract 3 chars starting at position 4 => "DEF"
           MOVE 4 TO WS-START.
           MOVE 3 TO WS-LEN.
           MOVE SPACES TO WS-RESULT.
           MOVE WS-DATA(WS-START:WS-LEN) TO WS-RESULT.
           IF WS-RESULT(1:3) = "DEF"
               DISPLAY "NC158A-TEST-1 PASS"
           ELSE
               DISPLAY "NC158A-TEST-1 FAIL"
               DISPLAY "  Expected DEF, got >" WS-RESULT "<"
           END-IF.
      * Test 2: Computed start position
      *   START = 2 * 5 = 10, LEN = 3 => chars 10..12 = "JKL"
           COMPUTE WS-START = 2 * 5.
           MOVE 3 TO WS-LEN.
           MOVE SPACES TO WS-RESULT.
           MOVE WS-DATA(WS-START:WS-LEN) TO WS-RESULT.
           IF WS-RESULT(1:3) = "JKL"
               DISPLAY "NC158A-TEST-2 PASS"
           ELSE
               DISPLAY "NC158A-TEST-2 FAIL"
               DISPLAY "  Expected JKL, got >" WS-RESULT "<"
           END-IF.
      * Test 3: Reference modification on receiving field (literal)
      *   Move "XYZ" into positions 1..3 of WS-RESULT
           MOVE "----------" TO WS-RESULT.
           MOVE "XYZ" TO WS-RESULT(1:3).
           IF WS-RESULT(1:3) = "XYZ"
               AND WS-RESULT(4:7) = "-------"
               DISPLAY "NC158A-TEST-3 PASS"
           ELSE
               DISPLAY "NC158A-TEST-3 FAIL"
               DISPLAY "  Expected XYZ-------, got >"
                   WS-RESULT "<"
           END-IF.
           STOP RUN.
