       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC203A.
      *
      * NIST CCVS-style test: UNSTRING with DELIMITER IN / COUNT IN
      * Tests UNSTRING with DELIMITER IN to capture the delimiter,
      * COUNT IN to capture the character count, and TALLYING IN
      * to count the number of receiving fields filled.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE       PIC X(30) VALUE SPACES.
       01 WS-SRC2         PIC X(7)  VALUE SPACES.
       01 WS-FIELD1       PIC X(10) VALUE SPACES.
       01 WS-FIELD2       PIC X(10) VALUE SPACES.
       01 WS-FIELD3       PIC X(10) VALUE SPACES.
       01 WS-DELIM1       PIC X(1)  VALUE SPACES.
       01 WS-DELIM2       PIC X(1)  VALUE SPACES.
       01 WS-COUNT1       PIC 9(2)  VALUE ZEROS.
       01 WS-COUNT2       PIC 9(2)  VALUE ZEROS.
       01 WS-COUNT3       PIC 9(2)  VALUE ZEROS.
       01 WS-TALLY        PIC 9(2)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: UNSTRING with DELIMITER IN
      *   "ABC,DEF,GHI" split by "," should capture delimiters
           MOVE "ABC,DEF,GHI" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           MOVE SPACES TO WS-DELIM1 WS-DELIM2.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-FIELD1 DELIMITER IN WS-DELIM1
                    WS-FIELD2 DELIMITER IN WS-DELIM2
                    WS-FIELD3.
           IF WS-FIELD1(1:3) = "ABC"
               AND WS-DELIM1 = ","
               AND WS-FIELD2(1:3) = "DEF"
               AND WS-DELIM2 = ","
               AND WS-FIELD3(1:3) = "GHI"
               DISPLAY "NC203A-TEST-1 PASS"
           ELSE
               DISPLAY "NC203A-TEST-1 FAIL"
               DISPLAY "  F1=>" WS-FIELD1 "< D1=>" WS-DELIM1 "<"
               DISPLAY "  F2=>" WS-FIELD2 "< D2=>" WS-DELIM2 "<"
               DISPLAY "  F3=>" WS-FIELD3 "<"
           END-IF.
      * Test 2: UNSTRING with COUNT IN - only 2 fields to avoid
      *   trailing spaces issue on last field
      *   "AB,CDEF" split by "," should give counts 2, 4
           MOVE "AB,CDEF" TO WS-SRC2.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2.
           MOVE 0 TO WS-COUNT1 WS-COUNT2.
           UNSTRING WS-SRC2 DELIMITED BY ","
               INTO WS-FIELD1 COUNT IN WS-COUNT1
                    WS-FIELD2 COUNT IN WS-COUNT2.
           IF WS-COUNT1 = 2
               AND WS-COUNT2 = 4
               DISPLAY "NC203A-TEST-2 PASS"
           ELSE
               DISPLAY "NC203A-TEST-2 FAIL"
               DISPLAY "  C1=" WS-COUNT1
                   " C2=" WS-COUNT2
           END-IF.
      * Test 3: UNSTRING with TALLYING IN
      *   "X;Y;Z" split by ";" fills 3 fields, tally should be 3
           MOVE "X;Y;Z" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SOURCE DELIMITED BY ";"
               INTO WS-FIELD1
                    WS-FIELD2
                    WS-FIELD3
               TALLYING IN WS-TALLY.
           IF WS-TALLY = 3
               AND WS-FIELD1(1:1) = "X"
               AND WS-FIELD2(1:1) = "Y"
               AND WS-FIELD3(1:1) = "Z"
               DISPLAY "NC203A-TEST-3 PASS"
           ELSE
               DISPLAY "NC203A-TEST-3 FAIL"
               DISPLAY "  TALLY=" WS-TALLY
               DISPLAY "  F1=>" WS-FIELD1 "<"
               DISPLAY "  F2=>" WS-FIELD2 "<"
               DISPLAY "  F3=>" WS-FIELD3 "<"
           END-IF.
           STOP RUN.
