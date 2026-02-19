       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC305A.
      *
      * NIST CCVS-style test: STRING with ON OVERFLOW
      * Tests that ON OVERFLOW triggers when the target field is
      * too small to hold all source data being strung together.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC1      PIC X(5) VALUE "ALPHA".
       01 WS-SRC2      PIC X(5) VALUE "BRAVO".
       01 WS-SRC3      PIC X(5) VALUE "CHARS".
       01 WS-SMALL     PIC X(8) VALUE SPACES.
       01 WS-PTR       PIC 99 VALUE 1.
       01 WS-FLAG      PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: STRING overflows small target (8 chars)
      *   "ALPHA" + "BRAVO" = 10 chars > 8, overflow fires
           MOVE SPACES TO WS-SMALL.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-FLAG.
           STRING WS-SRC1 DELIMITED BY SIZE
                  WS-SRC2 DELIMITED BY SIZE
               INTO WS-SMALL
               WITH POINTER WS-PTR
               ON OVERFLOW
                   MOVE 1 TO WS-FLAG
           END-STRING.
           IF WS-FLAG = 1
               DISPLAY "NC305A-TEST-1 PASS"
           ELSE
               DISPLAY "NC305A-TEST-1 FAIL"
               DISPLAY "  Expected overflow, FLAG=" WS-FLAG
           END-IF.
      * Test 2: STRING does NOT overflow when target is big enough
      *   "ALPHA" = 5 chars into 8-char target, no overflow
           MOVE SPACES TO WS-SMALL.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-FLAG.
           STRING WS-SRC1 DELIMITED BY SIZE
               INTO WS-SMALL
               WITH POINTER WS-PTR
               ON OVERFLOW
                   MOVE 1 TO WS-FLAG
           END-STRING.
           IF WS-FLAG = 0 AND WS-SMALL(1:5) = "ALPHA"
               DISPLAY "NC305A-TEST-2 PASS"
           ELSE
               DISPLAY "NC305A-TEST-2 FAIL"
               DISPLAY "  FLAG=" WS-FLAG " SMALL=>"
                   WS-SMALL "<"
           END-IF.
      * Test 3: STRING with three sources overflows
      *   "ALPHA" + "BRAVO" + "CHARS" = 15 chars > 8
           MOVE SPACES TO WS-SMALL.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-FLAG.
           STRING WS-SRC1 DELIMITED BY SIZE
                  WS-SRC2 DELIMITED BY SIZE
                  WS-SRC3 DELIMITED BY SIZE
               INTO WS-SMALL
               WITH POINTER WS-PTR
               ON OVERFLOW
                   MOVE 1 TO WS-FLAG
           END-STRING.
           IF WS-FLAG = 1 AND WS-SMALL(1:8) = "ALPHABRA"
               DISPLAY "NC305A-TEST-3 PASS"
           ELSE
               DISPLAY "NC305A-TEST-3 FAIL"
               DISPLAY "  FLAG=" WS-FLAG " SMALL=>"
                   WS-SMALL "<"
           END-IF.
           STOP RUN.
