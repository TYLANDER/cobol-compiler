       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC328A.
      *
      * NIST CCVS-style test: Group MOVE
      * Tests MOVE between group items (byte-level copy).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP-A.
          05 WS-A-PART1   PIC X(5) VALUE "HELLO".
          05 WS-A-PART2   PIC X(5) VALUE "WORLD".
       01 WS-GROUP-B.
          05 WS-B-PART1   PIC X(5) VALUE SPACES.
          05 WS-B-PART2   PIC X(5) VALUE SPACES.
       01 WS-GROUP-C.
          05 WS-C-PART1   PIC X(3) VALUE SPACES.
          05 WS-C-PART2   PIC X(3) VALUE SPACES.
          05 WS-C-PART3   PIC X(4) VALUE SPACES.
       01 WS-GROUP-D.
          05 WS-D-PART1   PIC X(4) VALUE "ABCD".
          05 WS-D-PART2   PIC X(4) VALUE "EFGH".
       01 WS-GROUP-E.
          05 WS-E-PART1   PIC X(4) VALUE SPACES.
          05 WS-E-PART2   PIC X(4) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Group MOVE copies bytes between equal-size groups
      *   WS-GROUP-B should become "HELLOWORLD"
           MOVE WS-GROUP-A TO WS-GROUP-B.
           IF WS-B-PART1 = "HELLO" AND WS-B-PART2 = "WORLD"
               DISPLAY "NC328A-TEST-1 PASS"
           ELSE
               DISPLAY "NC328A-TEST-1 FAIL"
               DISPLAY "  Expected 'HELLO' 'WORLD', got '"
                   WS-B-PART1 "' '" WS-B-PART2 "'"
           END-IF.
      * Test 2: Group MOVE to differently structured group
      *   Moving "HELLOWORLD" into 3+3+4 structure
           MOVE WS-GROUP-A TO WS-GROUP-C.
           IF WS-C-PART1 = "HEL" AND WS-C-PART2 = "LOW"
               AND WS-C-PART3 = "ORLD"
               DISPLAY "NC328A-TEST-2 PASS"
           ELSE
               DISPLAY "NC328A-TEST-2 FAIL"
               DISPLAY "  Expected 'HEL' 'LOW' 'ORLD'"
               DISPLAY "  Got '" WS-C-PART1 "' '"
                   WS-C-PART2 "' '" WS-C-PART3 "'"
           END-IF.
      * Test 3: Group MOVE with shorter source pads with spaces
      *   Moving "ABCDEFGH" (8 bytes) to 10-byte group
           MOVE SPACES TO WS-GROUP-B.
           MOVE WS-GROUP-D TO WS-GROUP-B.
           IF WS-B-PART1 = "ABCDE" AND WS-B-PART2 = "FGH  "
               DISPLAY "NC328A-TEST-3 PASS"
           ELSE
               DISPLAY "NC328A-TEST-3 FAIL"
               DISPLAY "  Expected 'ABCDE' 'FGH  ', got '"
                   WS-B-PART1 "' '" WS-B-PART2 "'"
           END-IF.
           STOP RUN.
