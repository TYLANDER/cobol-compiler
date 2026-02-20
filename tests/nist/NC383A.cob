       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC383A.
      *
      * NIST CCVS-style test: STRING with OVERFLOW handling
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC1            PIC X(10) VALUE "HELLO".
       01 WS-SRC2            PIC X(10) VALUE "WORLD".
       01 WS-DEST            PIC X(20) VALUE SPACES.
       01 WS-SMALL           PIC X(5)  VALUE SPACES.
       01 WS-PTR             PIC 99    VALUE 1.
       01 WS-OVERFLOW-FLAG   PIC 9     VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: STRING two fields into destination
           MOVE SPACES TO WS-DEST.
           MOVE 1 TO WS-PTR.
           STRING WS-SRC1 DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  WS-SRC2 DELIMITED BY SPACE
                  INTO WS-DEST
                  WITH POINTER WS-PTR.
           IF WS-DEST = "HELLO WORLD         "
               DISPLAY "NC383A-TEST-1 PASS"
           ELSE
               DISPLAY "NC383A-TEST-1 FAIL"
               DISPLAY "  Got [" WS-DEST "]"
           END-IF.
      * Test 2: STRING without overflow (fits exactly)
           MOVE SPACES TO WS-SMALL.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-OVERFLOW-FLAG.
           STRING "ABCDE" DELIMITED BY SIZE
                  INTO WS-SMALL
                  WITH POINTER WS-PTR
                  ON OVERFLOW
                      MOVE 1 TO WS-OVERFLOW-FLAG
           END-STRING.
           IF WS-OVERFLOW-FLAG = 0
               AND WS-SMALL = "ABCDE"
               DISPLAY "NC383A-TEST-2 PASS"
           ELSE
               DISPLAY "NC383A-TEST-2 FAIL"
               DISPLAY "  Flag=" WS-OVERFLOW-FLAG
               DISPLAY "  Got [" WS-SMALL "]"
           END-IF.
      * Test 3: STRING with overflow (too long)
           MOVE SPACES TO WS-SMALL.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-OVERFLOW-FLAG.
           STRING "ABCDEFGH" DELIMITED BY SIZE
                  INTO WS-SMALL
                  WITH POINTER WS-PTR
                  ON OVERFLOW
                      MOVE 1 TO WS-OVERFLOW-FLAG
           END-STRING.
           IF WS-OVERFLOW-FLAG = 1
               AND WS-SMALL = "ABCDE"
               DISPLAY "NC383A-TEST-3 PASS"
           ELSE
               DISPLAY "NC383A-TEST-3 FAIL"
               DISPLAY "  Flag=" WS-OVERFLOW-FLAG
               DISPLAY "  Got [" WS-SMALL "]"
           END-IF.
           STOP RUN.
