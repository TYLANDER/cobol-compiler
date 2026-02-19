       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC341A.
      *
      * NIST CCVS-style test: STRING with OVERFLOW/NOT ON OVERFLOW
      * Tests STRING ON OVERFLOW fires when target is too small,
      * and NOT ON OVERFLOW fires when target has room.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC1         PIC X(10) VALUE "AAAAAAAAAA".
       01 WS-SRC2         PIC X(10) VALUE "BBBBBBBBBB".
       01 WS-SRC3         PIC X(5)  VALUE "CCCCC".
       01 WS-SMALL        PIC X(8)  VALUE SPACES.
       01 WS-BIG          PIC X(30) VALUE SPACES.
       01 WS-PTR          PIC 99 VALUE 1.
       01 WS-FLAG         PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: STRING overflows - ON OVERFLOW fires
      *   "AAAAAAAAAA" + "BBBBBBBBBB" = 20 chars > 8
           MOVE SPACES TO WS-SMALL.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-FLAG.
           STRING WS-SRC1 DELIMITED BY SIZE
                  WS-SRC2 DELIMITED BY SIZE
               INTO WS-SMALL
               WITH POINTER WS-PTR
               ON OVERFLOW
                   MOVE 1 TO WS-FLAG
               NOT ON OVERFLOW
                   MOVE 2 TO WS-FLAG
           END-STRING.
           IF WS-FLAG = 1
               DISPLAY "NC341A-TEST-1 PASS"
           ELSE
               DISPLAY "NC341A-TEST-1 FAIL"
               DISPLAY "  Expected overflow, FLAG=" WS-FLAG
           END-IF.
      * Test 2: STRING fits - NOT ON OVERFLOW fires
      *   "CCCCC" = 5 chars into X(30), no overflow
           MOVE SPACES TO WS-BIG.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-FLAG.
           STRING WS-SRC3 DELIMITED BY SIZE
               INTO WS-BIG
               WITH POINTER WS-PTR
               ON OVERFLOW
                   MOVE 1 TO WS-FLAG
               NOT ON OVERFLOW
                   MOVE 2 TO WS-FLAG
           END-STRING.
           IF WS-FLAG = 2 AND WS-BIG(1:5) = "CCCCC"
               DISPLAY "NC341A-TEST-2 PASS"
           ELSE
               DISPLAY "NC341A-TEST-2 FAIL"
               DISPLAY "  FLAG=" WS-FLAG " BIG=>" WS-BIG "<"
           END-IF.
      * Test 3: STRING with three sources, just fits exactly
      *   "AAAAAAAAAA" + "BBBBBBBBBB" + "CCCCC" = 25 into X(30)
           MOVE SPACES TO WS-BIG.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-FLAG.
           STRING WS-SRC1 DELIMITED BY SIZE
                  WS-SRC2 DELIMITED BY SIZE
                  WS-SRC3 DELIMITED BY SIZE
               INTO WS-BIG
               WITH POINTER WS-PTR
               ON OVERFLOW
                   MOVE 1 TO WS-FLAG
               NOT ON OVERFLOW
                   MOVE 2 TO WS-FLAG
           END-STRING.
           IF WS-FLAG = 2 AND WS-PTR = 26
               DISPLAY "NC341A-TEST-3 PASS"
           ELSE
               DISPLAY "NC341A-TEST-3 FAIL"
               DISPLAY "  FLAG=" WS-FLAG " PTR=" WS-PTR
           END-IF.
           STOP RUN.
