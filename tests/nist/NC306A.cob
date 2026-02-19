       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC306A.
      *
      * NIST CCVS-style test: UNSTRING with ALL delimiter
      * Tests that consecutive delimiters are treated as one
      * logical separator when ALL is specified.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC       PIC X(30) VALUE SPACES.
       01 WS-F1        PIC X(10) VALUE SPACES.
       01 WS-F2        PIC X(10) VALUE SPACES.
       01 WS-F3        PIC X(10) VALUE SPACES.
       01 WS-TALLY     PIC 9(2) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: UNSTRING with ALL ","
      *   "HELLO,,,WORLD" => "HELLO" and "WORLD"
      *   Three consecutive commas treated as one delimiter
           MOVE "HELLO,,,WORLD" TO WS-SRC.
           MOVE SPACES TO WS-F1 WS-F2.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SRC DELIMITED BY ALL ","
               INTO WS-F1
                    WS-F2
               TALLYING IN WS-TALLY.
           IF WS-F1(1:5) = "HELLO"
               AND WS-F2(1:5) = "WORLD"
               AND WS-TALLY = 2
               DISPLAY "NC306A-TEST-1 PASS"
           ELSE
               DISPLAY "NC306A-TEST-1 FAIL"
               DISPLAY "  F1=>" WS-F1 "<"
               DISPLAY "  F2=>" WS-F2 "<"
               DISPLAY "  TALLY=" WS-TALLY
           END-IF.
      * Test 2: UNSTRING with ALL " " (spaces)
      *   "AA   BB   CC" => "AA", "BB", "CC"
           MOVE "AA   BB   CC" TO WS-SRC.
           MOVE SPACES TO WS-F1 WS-F2 WS-F3.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SRC DELIMITED BY ALL " "
               INTO WS-F1
                    WS-F2
                    WS-F3
               TALLYING IN WS-TALLY.
           IF WS-F1(1:2) = "AA"
               AND WS-F2(1:2) = "BB"
               AND WS-F3(1:2) = "CC"
               AND WS-TALLY = 3
               DISPLAY "NC306A-TEST-2 PASS"
           ELSE
               DISPLAY "NC306A-TEST-2 FAIL"
               DISPLAY "  F1=>" WS-F1 "<"
               DISPLAY "  F2=>" WS-F2 "<"
               DISPLAY "  F3=>" WS-F3 "<"
               DISPLAY "  TALLY=" WS-TALLY
           END-IF.
      * Test 3: UNSTRING with ALL ";" and single delimiter
      *   "X;Y" single ";" should still work with ALL
           MOVE "X;Y" TO WS-SRC.
           MOVE SPACES TO WS-F1 WS-F2.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SRC DELIMITED BY ALL ";"
               INTO WS-F1
                    WS-F2
               TALLYING IN WS-TALLY.
           IF WS-F1(1:1) = "X"
               AND WS-F2(1:1) = "Y"
               AND WS-TALLY = 2
               DISPLAY "NC306A-TEST-3 PASS"
           ELSE
               DISPLAY "NC306A-TEST-3 FAIL"
               DISPLAY "  F1=>" WS-F1 "<"
               DISPLAY "  F2=>" WS-F2 "<"
               DISPLAY "  TALLY=" WS-TALLY
           END-IF.
           STOP RUN.
