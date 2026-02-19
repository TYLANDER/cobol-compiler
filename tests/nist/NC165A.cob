       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC165A.
      *
      * NIST CCVS-style test: UNSTRING with single delimiter
      * Tests UNSTRING DELIMITED BY to split a string using
      * a single delimiter character.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE       PIC X(30) VALUE SPACES.
       01 WS-FIELD1       PIC X(10) VALUE SPACES.
       01 WS-FIELD2       PIC X(10) VALUE SPACES.
       01 WS-FIELD3       PIC X(10) VALUE SPACES.
       01 WS-TALLY        PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: UNSTRING with comma delimiter
      *   "ABC,DEF,GHI" split by ","
           MOVE "ABC,DEF,GHI" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           UNSTRING WS-SOURCE
               DELIMITED BY ","
               INTO WS-FIELD1
                    WS-FIELD2
                    WS-FIELD3.
           IF WS-FIELD1(1:3) = "ABC"
               AND WS-FIELD2(1:3) = "DEF"
               AND WS-FIELD3(1:3) = "GHI"
               DISPLAY "NC165A-TEST-1 PASS"
           ELSE
               DISPLAY "NC165A-TEST-1 FAIL"
               DISPLAY "  F1=>" WS-FIELD1 "<"
               DISPLAY "  F2=>" WS-FIELD2 "<"
               DISPLAY "  F3=>" WS-FIELD3 "<"
           END-IF.
      * Test 2: UNSTRING with space delimiter
      *   "HELLO WORLD COBOL" split by " "
           MOVE "HELLO WORLD COBOL" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           UNSTRING WS-SOURCE
               DELIMITED BY " "
               INTO WS-FIELD1
                    WS-FIELD2
                    WS-FIELD3.
           IF WS-FIELD1(1:5) = "HELLO"
               AND WS-FIELD2(1:5) = "WORLD"
               AND WS-FIELD3(1:5) = "COBOL"
               DISPLAY "NC165A-TEST-2 PASS"
           ELSE
               DISPLAY "NC165A-TEST-2 FAIL"
               DISPLAY "  F1=>" WS-FIELD1 "<"
               DISPLAY "  F2=>" WS-FIELD2 "<"
               DISPLAY "  F3=>" WS-FIELD3 "<"
           END-IF.
           STOP RUN.
