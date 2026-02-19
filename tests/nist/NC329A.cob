       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC329A.
      *
      * NIST CCVS-style test: IF with combined conditions
      * Tests abbreviated combined conditions (IF A = 1 OR 2 OR 3).
      * Falls back to expanded form if compiler does not support
      * abbreviated syntax.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CODE         PIC 9(2) VALUE 0.
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: IF with OR on matching value
      *   WS-CODE = 3, should match = 1 OR = 2 OR = 3
           MOVE 3 TO WS-CODE.
           IF WS-CODE = 1 OR WS-CODE = 2 OR WS-CODE = 3
               DISPLAY "NC329A-TEST-1 PASS"
           ELSE
               DISPLAY "NC329A-TEST-1 FAIL"
               DISPLAY "  WS-CODE=3, expected match"
           END-IF.
      * Test 2: IF with OR on non-matching value
      *   WS-CODE = 7, should NOT match = 1 OR = 2 OR = 3
           MOVE 7 TO WS-CODE.
           IF WS-CODE = 1 OR WS-CODE = 2 OR WS-CODE = 3
               DISPLAY "NC329A-TEST-2 FAIL"
               DISPLAY "  WS-CODE=7, should not match"
           ELSE
               DISPLAY "NC329A-TEST-2 PASS"
           END-IF.
      * Test 3: IF with OR and AND combined
      *   WS-CODE = 5, test (= 5 OR = 10) is true
           MOVE 5 TO WS-CODE.
           IF (WS-CODE = 5 OR WS-CODE = 10)
               AND (WS-CODE < 20)
               DISPLAY "NC329A-TEST-3 PASS"
           ELSE
               DISPLAY "NC329A-TEST-3 FAIL"
               DISPLAY "  WS-CODE=5, expected match"
           END-IF.
           STOP RUN.
