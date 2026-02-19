       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC281A.
      *
      * NIST CCVS-style test: STRING with multiple source fields
      * and SIZE delimiter. Tests STRING concatenation using
      * DELIMITED BY SIZE with multiple sending fields.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PART1        PIC X(5)  VALUE "HELLO".
       01 WS-PART2        PIC X(1)  VALUE " ".
       01 WS-PART3        PIC X(5)  VALUE "WORLD".
       01 WS-DEST         PIC X(20) VALUE SPACES.
       01 WS-PTR          PIC 9(4)  VALUE 1.
       PROCEDURE DIVISION.
      * Test 1: STRING three fields DELIMITED BY SIZE
      *   "HELLO" + " " + "WORLD" => "HELLO WORLD"
           MOVE SPACES TO WS-DEST.
           MOVE 1 TO WS-PTR.
           STRING WS-PART1 DELIMITED BY SIZE
                  WS-PART2 DELIMITED BY SIZE
                  WS-PART3 DELIMITED BY SIZE
             INTO WS-DEST
             WITH POINTER WS-PTR.
           IF WS-DEST(1:11) = "HELLO WORLD"
               AND WS-PTR = 12
               DISPLAY "NC281A-TEST-1 PASS"
           ELSE
               DISPLAY "NC281A-TEST-1 FAIL"
               DISPLAY "  Expected HELLO WORLD, got >"
                   WS-DEST "<"
               DISPLAY "  PTR=" WS-PTR
           END-IF.
      * Test 2: STRING with shared DELIMITED BY SIZE clause
      *   "HELLO" + "WORLD" => "HELLOWORLD"
           MOVE SPACES TO WS-DEST.
           MOVE 1 TO WS-PTR.
           STRING WS-PART1 WS-PART3
               DELIMITED BY SIZE
               INTO WS-DEST
               WITH POINTER WS-PTR.
           IF WS-DEST(1:10) = "HELLOWORLD"
               AND WS-PTR = 11
               DISPLAY "NC281A-TEST-2 PASS"
           ELSE
               DISPLAY "NC281A-TEST-2 FAIL"
               DISPLAY "  Expected HELLOWORLD, got >"
                   WS-DEST "<"
               DISPLAY "  PTR=" WS-PTR
           END-IF.
      * Test 3: STRING with literals and field mixed
      *   "***" + "HELLO" + "***" => "***HELLO***"
           MOVE SPACES TO WS-DEST.
           MOVE 1 TO WS-PTR.
           STRING "***" DELIMITED BY SIZE
                  WS-PART1 DELIMITED BY SIZE
                  "***" DELIMITED BY SIZE
               INTO WS-DEST
               WITH POINTER WS-PTR.
           IF WS-DEST(1:11) = "***HELLO***"
               AND WS-PTR = 12
               DISPLAY "NC281A-TEST-3 PASS"
           ELSE
               DISPLAY "NC281A-TEST-3 FAIL"
               DISPLAY "  Expected ***HELLO***, got >"
                   WS-DEST "<"
               DISPLAY "  PTR=" WS-PTR
           END-IF.
           STOP RUN.
