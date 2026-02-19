       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC111A.
      *
      * NIST CCVS-style test: STRING statement
      * Tests STRING with POINTER, DELIMITED BY SIZE,
      * and DELIMITED BY literal.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIRST    PIC X(5) VALUE "HELLO".
       01 WS-SECOND   PIC X(5) VALUE "WORLD".
       01 WS-TARGET   PIC X(20) VALUE "                    ".
       01 WS-PTR      PIC 9(2) VALUE 1.
       PROCEDURE DIVISION.
      * Test 1: STRING with POINTER (fields)
           MOVE "                    " TO WS-TARGET.
           MOVE 1 TO WS-PTR.
           STRING WS-FIRST DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-SECOND DELIMITED BY SIZE
             INTO WS-TARGET
             WITH POINTER WS-PTR.
           IF WS-TARGET = "HELLOWORLD         "
               DISPLAY "NC111A-TEST-1 PASS"
           ELSE
               IF WS-TARGET = "HELLO WORLD         "
                   DISPLAY "NC111A-TEST-1 PASS"
               ELSE
                   DISPLAY "NC111A-TEST-1 FAIL"
                   DISPLAY "  Got >" WS-TARGET "<"
               END-IF
           END-IF.
      * Test 2: STRING with DELIMITED BY SIZE (with POINTER)
           MOVE "                    " TO WS-TARGET.
           MOVE 1 TO WS-PTR.
           STRING "ABC" DELIMITED BY SIZE
                  "DEF" DELIMITED BY SIZE
             INTO WS-TARGET
             WITH POINTER WS-PTR.
           IF WS-TARGET(1:6) = "ABCDEF"
               DISPLAY "NC111A-TEST-2 PASS"
           ELSE
               DISPLAY "NC111A-TEST-2 FAIL"
               DISPLAY "  Got >" WS-TARGET "<"
           END-IF.
      * Test 3: STRING with DELIMITED BY literal
           MOVE "                    " TO WS-TARGET.
           MOVE 1 TO WS-PTR.
           STRING "HELLO WORLD" DELIMITED BY " "
             INTO WS-TARGET
             WITH POINTER WS-PTR.
           IF WS-TARGET(1:5) = "HELLO"
               DISPLAY "NC111A-TEST-3 PASS"
           ELSE
               DISPLAY "NC111A-TEST-3 FAIL"
               DISPLAY "  Got >" WS-TARGET "<"
           END-IF.
           STOP RUN.
