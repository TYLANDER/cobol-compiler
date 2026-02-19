       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC187A.
      *
      * NIST CCVS-style test: Multiple targets in MOVE
      * Tests MOVE with multiple receiving fields in a single
      * statement (MOVE X TO A B C).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE         PIC X(5) VALUE SPACES.
       01 WS-DEST-A         PIC X(5) VALUE SPACES.
       01 WS-DEST-B         PIC X(5) VALUE SPACES.
       01 WS-DEST-C         PIC X(5) VALUE SPACES.
       01 WS-NUM-SRC        PIC 9(4) VALUE ZEROS.
       01 WS-NUM-A          PIC 9(4) VALUE ZEROS.
       01 WS-NUM-B          PIC 9(4) VALUE ZEROS.
       01 WS-NUM-C          PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: MOVE alphanumeric to three targets
      *   MOVE "ABCDE" TO A B C => all three should be "ABCDE"
           MOVE "ABCDE" TO WS-DEST-A WS-DEST-B WS-DEST-C.
           IF WS-DEST-A = "ABCDE"
               AND WS-DEST-B = "ABCDE"
               AND WS-DEST-C = "ABCDE"
               DISPLAY "NC187A-TEST-1 PASS"
           ELSE
               DISPLAY "NC187A-TEST-1 FAIL"
               DISPLAY "  A=>" WS-DEST-A "<"
               DISPLAY "  B=>" WS-DEST-B "<"
               DISPLAY "  C=>" WS-DEST-C "<"
           END-IF.
      * Test 2: MOVE numeric to three targets
      *   MOVE 9999 TO A B C => all three should be 9999
           MOVE 9999 TO WS-NUM-A WS-NUM-B WS-NUM-C.
           IF WS-NUM-A = 9999
               AND WS-NUM-B = 9999
               AND WS-NUM-C = 9999
               DISPLAY "NC187A-TEST-2 PASS"
           ELSE
               DISPLAY "NC187A-TEST-2 FAIL"
               DISPLAY "  A=" WS-NUM-A
               DISPLAY "  B=" WS-NUM-B
               DISPLAY "  C=" WS-NUM-C
           END-IF.
      * Test 3: MOVE SPACES to multiple alphanumeric targets
      *   Verify all three are cleared
           MOVE "XXXXX" TO WS-DEST-A.
           MOVE "YYYYY" TO WS-DEST-B.
           MOVE "ZZZZZ" TO WS-DEST-C.
           MOVE SPACES TO WS-DEST-A WS-DEST-B WS-DEST-C.
           IF WS-DEST-A = SPACES
               AND WS-DEST-B = SPACES
               AND WS-DEST-C = SPACES
               DISPLAY "NC187A-TEST-3 PASS"
           ELSE
               DISPLAY "NC187A-TEST-3 FAIL"
               DISPLAY "  A=>" WS-DEST-A "<"
               DISPLAY "  B=>" WS-DEST-B "<"
               DISPLAY "  C=>" WS-DEST-C "<"
           END-IF.
           STOP RUN.
