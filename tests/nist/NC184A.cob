       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC184A.
      *
      * NIST CCVS-style test: STRING with multiple sources and POINTER
      * Tests STRING statement concatenating multiple source fields
      * with DELIMITED BY and using the POINTER clause.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC1          PIC X(5)  VALUE "HELLO".
       01 WS-SRC2          PIC X(5)  VALUE "WORLD".
       01 WS-SRC3          PIC X(5)  VALUE "COBOL".
       01 WS-DEST          PIC X(30) VALUE SPACES.
       01 WS-PTR           PIC 9(2)  VALUE 1.
       PROCEDURE DIVISION.
      * Test 1: STRING two sources with space separator
      *   "HELLO" + " " + "WORLD" => "HELLO WORLD"
           MOVE SPACES TO WS-DEST.
           MOVE 1 TO WS-PTR.
           STRING WS-SRC1 DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-SRC2 DELIMITED BY SIZE
               INTO WS-DEST
               WITH POINTER WS-PTR.
           IF WS-DEST(1:11) = "HELLO WORLD"
               DISPLAY "NC184A-TEST-1 PASS"
           ELSE
               DISPLAY "NC184A-TEST-1 FAIL"
               DISPLAY "  Expected 'HELLO WORLD', got >"
                   WS-DEST "<"
           END-IF.
      * Test 2: Verify POINTER value after STRING
      *   After "HELLO WORLD" (11 chars), PTR should be 12
           IF WS-PTR = 12
               DISPLAY "NC184A-TEST-2 PASS"
           ELSE
               DISPLAY "NC184A-TEST-2 FAIL"
               DISPLAY "  Expected PTR=12, got " WS-PTR
           END-IF.
      * Test 3: STRING three sources with POINTER continuation
      *   Continue appending " COBOL" at current pointer position
           STRING " " DELIMITED BY SIZE
                  WS-SRC3 DELIMITED BY SIZE
               INTO WS-DEST
               WITH POINTER WS-PTR.
           IF WS-DEST(1:17) = "HELLO WORLD COBOL"
               DISPLAY "NC184A-TEST-3 PASS"
           ELSE
               DISPLAY "NC184A-TEST-3 FAIL"
               DISPLAY "  Expected 'HELLO WORLD COBOL'"
               DISPLAY "  Got >" WS-DEST "<"
           END-IF.
           STOP RUN.
