       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC143A.
      *
      * NIST CCVS-style test: STRING with DELIMITED BY SIZE
      * Tests the STRING statement using DELIMITED BY SIZE
      * to concatenate fields without truncation at delimiters.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC1         PIC X(5)  VALUE "HELLO".
       01 WS-SRC2         PIC X(5)  VALUE "WORLD".
       01 WS-SRC3         PIC X(3)  VALUE "!!!".
       01 WS-DEST         PIC X(20) VALUE SPACES.
       01 WS-PTR          PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: STRING two fields DELIMITED BY SIZE
      *   "HELLO" + "WORLD" = "HELLOWORLD"
           MOVE SPACES TO WS-DEST.
           STRING WS-SRC1 DELIMITED BY SIZE
               WS-SRC2 DELIMITED BY SIZE
               INTO WS-DEST.
           IF WS-DEST(1:10) = "HELLOWORLD"
               DISPLAY "NC143A-TEST-1 PASS"
           ELSE
               DISPLAY "NC143A-TEST-1 FAIL"
               DISPLAY "  Expected HELLOWORLD, got >"
                   WS-DEST "<"
           END-IF.
      * Test 2: STRING three fields DELIMITED BY SIZE
      *   "HELLO" + "WORLD" + "!!!" = "HELLOWORLD!!!"
           MOVE SPACES TO WS-DEST.
           STRING WS-SRC1 DELIMITED BY SIZE
               WS-SRC2 DELIMITED BY SIZE
               WS-SRC3 DELIMITED BY SIZE
               INTO WS-DEST.
           IF WS-DEST(1:13) = "HELLOWORLD!!!"
               DISPLAY "NC143A-TEST-2 PASS"
           ELSE
               DISPLAY "NC143A-TEST-2 FAIL"
               DISPLAY "  Expected HELLOWORLD!!!, got >"
                   WS-DEST "<"
           END-IF.
      * Test 3: STRING with POINTER
      *   Start writing at position 4 of destination
           MOVE SPACES TO WS-DEST.
           MOVE 4 TO WS-PTR.
           STRING WS-SRC1 DELIMITED BY SIZE
               INTO WS-DEST
               WITH POINTER WS-PTR.
           IF WS-DEST(4:5) = "HELLO" AND WS-PTR = 9
               DISPLAY "NC143A-TEST-3 PASS"
           ELSE
               DISPLAY "NC143A-TEST-3 FAIL"
               DISPLAY "  Expected HELLO at pos 4, PTR=9"
               DISPLAY "  Got >" WS-DEST "< PTR=" WS-PTR
           END-IF.
           STOP RUN.
