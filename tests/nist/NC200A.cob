       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC200A.
      *
      * NIST CCVS-style test: STRING with multiple DELIMITED BY
      * Tests STRING statement with several source items each
      * using different DELIMITED BY clauses chained together
      * in a single STRING statement.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC1       PIC X(10) VALUE "HELLO     ".
       01 WS-SRC2       PIC X(10) VALUE "WORLD     ".
       01 WS-SRC3       PIC X(10) VALUE "COBOL/TEST".
       01 WS-TARGET     PIC X(30) VALUE SPACES.
       01 WS-PTR        PIC 9(2)  VALUE 1.
       01 WS-PART1      PIC X(10) VALUE "ABC DEF".
       01 WS-PART2      PIC X(10) VALUE "GHI,JKL".
       01 WS-RESULT     PIC X(30) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: STRING three fields, each DELIMITED BY SPACE
      *   "HELLO     " delimited by space => "HELLO"
      *   "WORLD     " delimited by space => "WORLD"
      *   literal "-END" delimited by size => "-END"
      *   Result: "HELLOWORLD-END"
           MOVE SPACES TO WS-TARGET.
           MOVE 1 TO WS-PTR.
           STRING WS-SRC1 DELIMITED BY " "
                  WS-SRC2 DELIMITED BY " "
                  "-END" DELIMITED BY SIZE
             INTO WS-TARGET
             WITH POINTER WS-PTR.
           IF WS-TARGET(1:14) = "HELLOWORLD-END"
               DISPLAY "NC200A-TEST-1 PASS"
           ELSE
               DISPLAY "NC200A-TEST-1 FAIL"
               DISPLAY "  Expected HELLOWORLD-END, got >"
                   WS-TARGET "<"
           END-IF.
      * Test 2: STRING with mixed delimiter types
      *   "ABC DEF" delimited by space => "ABC"
      *   "GHI,JKL" delimited by "," => "GHI"
      *   "!" delimited by size => "!"
      *   Result: "ABCGHI!"
           MOVE SPACES TO WS-RESULT.
           MOVE 1 TO WS-PTR.
           STRING WS-PART1 DELIMITED BY " "
                  WS-PART2 DELIMITED BY ","
                  "!" DELIMITED BY SIZE
             INTO WS-RESULT
             WITH POINTER WS-PTR.
           IF WS-RESULT(1:7) = "ABCGHI!"
               DISPLAY "NC200A-TEST-2 PASS"
           ELSE
               DISPLAY "NC200A-TEST-2 FAIL"
               DISPLAY "  Expected ABCGHI!, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 3: STRING with DELIMITED BY "/" (slash character)
      *   "COBOL/TEST" delimited by "/" => "COBOL"
      *   Followed by literal "85" delimited by size
      *   Result: "COBOL85"
           MOVE SPACES TO WS-TARGET.
           MOVE 1 TO WS-PTR.
           STRING WS-SRC3 DELIMITED BY "/"
                  "85" DELIMITED BY SIZE
             INTO WS-TARGET
             WITH POINTER WS-PTR.
           IF WS-TARGET(1:7) = "COBOL85"
               DISPLAY "NC200A-TEST-3 PASS"
           ELSE
               DISPLAY "NC200A-TEST-3 FAIL"
               DISPLAY "  Expected COBOL85, got >"
                   WS-TARGET "<"
           END-IF.
           STOP RUN.
