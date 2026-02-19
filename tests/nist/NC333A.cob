       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC333A.
      *
      * NIST CCVS-style test: Reference modification on group items.
      * Tests extracting substrings from group items using the
      * reference modification syntax: identifier(start:length).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP-A.
          05 WS-A-PART1   PIC X(5) VALUE "HELLO".
          05 WS-A-PART2   PIC X(5) VALUE "WORLD".
       01 WS-GROUP-B.
          05 WS-B-NUM      PIC 9(3) VALUE 123.
          05 WS-B-TEXT     PIC X(7) VALUE "ABCDEFG".
       01 WS-RESULT        PIC X(10) VALUE SPACES.
       01 WS-SUB-RESULT    PIC X(5) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Reference modification on group item
      *   WS-GROUP-A is "HELLOWORLD" (10 bytes).
      *   WS-GROUP-A(3:5) => bytes 3-7 => "LLOWO"
           MOVE WS-GROUP-A(3:5) TO WS-SUB-RESULT.
           IF WS-SUB-RESULT = "LLOWO"
               DISPLAY "NC333A-TEST-1 PASS"
           ELSE
               DISPLAY "NC333A-TEST-1 FAIL"
               DISPLAY "  Expected 'LLOWO', got '"
                   WS-SUB-RESULT "'"
           END-IF.
      * Test 2: Reference modification spanning sub-fields
      *   WS-GROUP-A(4:4) => bytes 4-7 => "LOWO"
           MOVE SPACES TO WS-SUB-RESULT.
           MOVE WS-GROUP-A(4:4) TO WS-SUB-RESULT.
           IF WS-SUB-RESULT = "LOWO "
               DISPLAY "NC333A-TEST-2 PASS"
           ELSE
               DISPLAY "NC333A-TEST-2 FAIL"
               DISPLAY "  Expected 'LOWO ', got '"
                   WS-SUB-RESULT "'"
           END-IF.
      * Test 3: Reference modification on group with numeric field
      *   WS-GROUP-B is "123ABCDEFG" (10 bytes).
      *   WS-GROUP-B(2:6) => bytes 2-7 => "23ABCD"
           MOVE SPACES TO WS-RESULT.
           MOVE WS-GROUP-B(2:6) TO WS-RESULT.
           IF WS-RESULT = "23ABCD    "
               DISPLAY "NC333A-TEST-3 PASS"
           ELSE
               DISPLAY "NC333A-TEST-3 FAIL"
               DISPLAY "  Expected '23ABCD    ', got '"
                   WS-RESULT "'"
           END-IF.
           STOP RUN.
