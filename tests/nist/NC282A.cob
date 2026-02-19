       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC282A.
      *
      * NIST CCVS-style test: UNSTRING with ALL delimiter
      * Tests UNSTRING using ALL to treat consecutive delimiters
      * as a single delimiter, plus TALLYING and COUNT IN.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE       PIC X(30) VALUE SPACES.
       01 WS-FIELD1       PIC X(10) VALUE SPACES.
       01 WS-FIELD2       PIC X(10) VALUE SPACES.
       01 WS-FIELD3       PIC X(10) VALUE SPACES.
       01 WS-TALLY        PIC 9(2)  VALUE ZEROS.
       01 WS-CNT1         PIC 9(2)  VALUE ZEROS.
       01 WS-CNT2         PIC 9(2)  VALUE ZEROS.
       01 WS-DELIM1       PIC X(3)  VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: UNSTRING with ALL "," delimiter
      *   "XX,,,,YY,ZZ" with ALL "," => "XX" and "YY" and "ZZ"
           MOVE "XX,,,,YY,ZZ" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SOURCE DELIMITED BY ALL ","
               INTO WS-FIELD1
                    WS-FIELD2
                    WS-FIELD3
               TALLYING IN WS-TALLY.
           IF WS-FIELD1(1:2) = "XX"
               AND WS-FIELD2(1:2) = "YY"
               AND WS-FIELD3(1:2) = "ZZ"
               AND WS-TALLY = 3
               DISPLAY "NC282A-TEST-1 PASS"
           ELSE
               DISPLAY "NC282A-TEST-1 FAIL"
               DISPLAY "  F1=>" WS-FIELD1 "<"
               DISPLAY "  F2=>" WS-FIELD2 "<"
               DISPLAY "  F3=>" WS-FIELD3 "<"
               DISPLAY "  TALLY=" WS-TALLY
           END-IF.
      * Test 2: UNSTRING with DELIMITER IN
      *   "HELLO-WORLD" split by "-" and capture the delimiter
           MOVE "HELLO-WORLD" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2 WS-DELIM1.
           UNSTRING WS-SOURCE DELIMITED BY "-"
               INTO WS-FIELD1 DELIMITER IN WS-DELIM1
                    WS-FIELD2.
           IF WS-FIELD1(1:5) = "HELLO"
               AND WS-DELIM1(1:1) = "-"
               AND WS-FIELD2(1:5) = "WORLD"
               DISPLAY "NC282A-TEST-2 PASS"
           ELSE
               DISPLAY "NC282A-TEST-2 FAIL"
               DISPLAY "  F1=>" WS-FIELD1 "< D1=>"
                   WS-DELIM1 "<"
               DISPLAY "  F2=>" WS-FIELD2 "<"
           END-IF.
      * Test 3: UNSTRING with COUNT IN
      *   "ABCDE,FG" split by "," with count
           MOVE "ABCDE,FG" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2.
           MOVE 0 TO WS-CNT1 WS-CNT2.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-FIELD1 COUNT IN WS-CNT1
                    WS-FIELD2 COUNT IN WS-CNT2.
           IF WS-FIELD1(1:5) = "ABCDE"
               AND WS-CNT1 = 5
               AND WS-FIELD2(1:2) = "FG"
               DISPLAY "NC282A-TEST-3 PASS"
           ELSE
               DISPLAY "NC282A-TEST-3 FAIL"
               DISPLAY "  F1=>" WS-FIELD1 "<"
               DISPLAY "  CNT1=" WS-CNT1
               DISPLAY "  F2=>" WS-FIELD2 "<"
               DISPLAY "  CNT2=" WS-CNT2
           END-IF.
           STOP RUN.
