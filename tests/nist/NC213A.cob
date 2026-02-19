       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC213A.
      *
      * NIST CCVS-style test: STRING with OVERFLOW clause
      * Tests STRING concatenation with WITH POINTER and
      * ON OVERFLOW / NOT ON OVERFLOW handling.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DEST         PIC X(20) VALUE SPACES.
       01 WS-SHORT-DEST   PIC X(5)  VALUE SPACES.
       01 WS-PTR           PIC 9(4) VALUE 1.
       01 WS-PART1        PIC X(5)  VALUE "HELLO".
       01 WS-PART2        PIC X(5)  VALUE "WORLD".
       01 WS-PART3        PIC X(5)  VALUE "ABCDE".
       01 WS-OVFL-FLAG    PIC 9     VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: STRING that fits - NOT ON OVERFLOW fires
      *   "HELLO" + "WORLD" = "HELLOWORLD" into X(20)
           MOVE SPACES TO WS-DEST.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-OVFL-FLAG.
           STRING WS-PART1 WS-PART2
               DELIMITED BY SIZE
               INTO WS-DEST
               WITH POINTER WS-PTR
               ON OVERFLOW
                   MOVE 1 TO WS-OVFL-FLAG
               NOT ON OVERFLOW
                   MOVE 2 TO WS-OVFL-FLAG
           END-STRING.
           IF WS-DEST(1:10) = "HELLOWORLD"
               AND WS-OVFL-FLAG = 2
               AND WS-PTR = 11
               DISPLAY "NC213A-TEST-1 PASS"
           ELSE
               DISPLAY "NC213A-TEST-1 FAIL"
               DISPLAY "  DEST=>" WS-DEST "<"
               DISPLAY "  FLAG=" WS-OVFL-FLAG
                   " PTR=" WS-PTR
           END-IF.
      * Test 2: STRING that overflows - ON OVERFLOW fires
      *   "HELLO" + "WORLD" + "ABCDE" into X(5) overflows
           MOVE SPACES TO WS-SHORT-DEST.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-OVFL-FLAG.
           STRING WS-PART1 WS-PART2
               DELIMITED BY SIZE
               INTO WS-SHORT-DEST
               WITH POINTER WS-PTR
               ON OVERFLOW
                   MOVE 1 TO WS-OVFL-FLAG
               NOT ON OVERFLOW
                   MOVE 2 TO WS-OVFL-FLAG
           END-STRING.
           IF WS-OVFL-FLAG = 1
               AND WS-SHORT-DEST = "HELLO"
               DISPLAY "NC213A-TEST-2 PASS"
           ELSE
               DISPLAY "NC213A-TEST-2 FAIL"
               DISPLAY "  DEST=>" WS-SHORT-DEST "<"
               DISPLAY "  FLAG=" WS-OVFL-FLAG
           END-IF.
      * Test 3: STRING with POINTER starting mid-destination
      *   Start at position 4, write "ABC" into X(20)
           MOVE SPACES TO WS-DEST.
           MOVE 4 TO WS-PTR.
           MOVE 0 TO WS-OVFL-FLAG.
           STRING "ABC"
               DELIMITED BY SIZE
               INTO WS-DEST
               WITH POINTER WS-PTR
               NOT ON OVERFLOW
                   MOVE 2 TO WS-OVFL-FLAG
           END-STRING.
           IF WS-DEST(4:3) = "ABC"
               AND WS-PTR = 7
               AND WS-OVFL-FLAG = 2
               DISPLAY "NC213A-TEST-3 PASS"
           ELSE
               DISPLAY "NC213A-TEST-3 FAIL"
               DISPLAY "  DEST=>" WS-DEST "<"
               DISPLAY "  PTR=" WS-PTR
                   " FLAG=" WS-OVFL-FLAG
           END-IF.
           STOP RUN.
