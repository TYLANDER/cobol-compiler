       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC117A.
      *
      * NIST CCVS-style test: STRING and UNSTRING operations
      * Tests STRING DELIMITED BY SIZE, STRING DELIMITED BY SPACE,
      * and UNSTRING with DELIMITED BY comma.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIRST     PIC X(5) VALUE "ALPHA".
       01 WS-SECOND    PIC X(4) VALUE "BETA".
       01 WS-THIRD     PIC X(5) VALUE "GAMMA".
       01 WS-TARGET    PIC X(30) VALUE SPACES.
       01 WS-PTR       PIC 9(2) VALUE 1.
       01 WS-SOURCE    PIC X(20) VALUE SPACES.
       01 WS-PART1     PIC X(10) VALUE SPACES.
       01 WS-PART2     PIC X(10) VALUE SPACES.
       01 WS-PART3     PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: STRING multiple fields DELIMITED BY SIZE INTO result
           MOVE SPACES TO WS-TARGET.
           MOVE 1 TO WS-PTR.
           STRING WS-FIRST DELIMITED BY SIZE
                  WS-SECOND DELIMITED BY SIZE
                  WS-THIRD DELIMITED BY SIZE
             INTO WS-TARGET
             WITH POINTER WS-PTR.
           IF WS-TARGET(1:14) = "ALPHABETAGAMMA"
               DISPLAY "NC117A-TEST-1 PASS"
           ELSE
               DISPLAY "NC117A-TEST-1 FAIL"
               DISPLAY "  Expected ALPHABETAGAMMA, got >"
                   WS-TARGET "<"
           END-IF.
      * Test 2: STRING with DELIMITED BY SPACE
      *   "HELLO WORLD" delimited by space should give "HELLO"
           MOVE SPACES TO WS-TARGET.
           MOVE 1 TO WS-PTR.
           STRING "HELLO WORLD" DELIMITED BY " "
             INTO WS-TARGET
             WITH POINTER WS-PTR.
           IF WS-TARGET(1:5) = "HELLO"
               DISPLAY "NC117A-TEST-2 PASS"
           ELSE
               DISPLAY "NC117A-TEST-2 FAIL"
               DISPLAY "  Expected HELLO, got >" WS-TARGET "<"
           END-IF.
      * Test 3: UNSTRING with DELIMITED BY comma
           MOVE "AAA,BBB,CCC" TO WS-SOURCE.
           MOVE SPACES TO WS-PART1.
           MOVE SPACES TO WS-PART2.
           MOVE SPACES TO WS-PART3.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-PART1 WS-PART2 WS-PART3.
           IF WS-PART1(1:3) = "AAA" AND
              WS-PART2(1:3) = "BBB" AND
              WS-PART3(1:3) = "CCC"
               DISPLAY "NC117A-TEST-3 PASS"
           ELSE
               DISPLAY "NC117A-TEST-3 FAIL"
               DISPLAY "  Part1=>" WS-PART1 "<"
               DISPLAY "  Part2=>" WS-PART2 "<"
               DISPLAY "  Part3=>" WS-PART3 "<"
           END-IF.
           STOP RUN.
