       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC260A.
      *
      * NIST CCVS-style test: UNSTRING with DELIMITER IN / COUNT IN
      * / POINTER / TALLYING
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE       PIC X(30) VALUE SPACES.
       01 WS-SRC-SHORT    PIC X(11) VALUE SPACES.
       01 WS-FIELD1       PIC X(10) VALUE SPACES.
       01 WS-FIELD2       PIC X(10) VALUE SPACES.
       01 WS-FIELD3       PIC X(10) VALUE SPACES.
       01 WS-DELIM1       PIC X(3)  VALUE SPACES.
       01 WS-DELIM2       PIC X(3)  VALUE SPACES.
       01 WS-COUNT1       PIC 9(2)  VALUE ZEROS.
       01 WS-COUNT2       PIC 9(2)  VALUE ZEROS.
       01 WS-PTR          PIC 9(2)  VALUE 1.
       01 WS-TALLY        PIC 9(2)  VALUE ZEROS.
       PROCEDURE DIVISION.
       NC260A-CONTROL.
           PERFORM NC260A-TEST-1.
           PERFORM NC260A-TEST-2.
           PERFORM NC260A-TEST-3.
           STOP RUN.
       NC260A-TEST-1.
      * UNSTRING with DELIMITER IN and COUNT IN
      * "HELLO,WORLD" (11 chars) delimited by "," =>
      *   FIELD1="HELLO", DELIM1=",", COUNT1=5
      *   FIELD2="WORLD", COUNT2=5
           MOVE "HELLO,WORLD" TO WS-SRC-SHORT.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2.
           MOVE SPACES TO WS-DELIM1.
           MOVE 0 TO WS-COUNT1 WS-COUNT2.
           UNSTRING WS-SRC-SHORT DELIMITED BY ","
               INTO WS-FIELD1
                   DELIMITER IN WS-DELIM1
                   COUNT IN WS-COUNT1
               WS-FIELD2
                   COUNT IN WS-COUNT2.
           IF WS-FIELD1 = "HELLO     "
               AND WS-DELIM1 = ",  "
               AND WS-COUNT1 = 5
               AND WS-COUNT2 = 5
               DISPLAY "NC260A-TEST-1 PASS"
           ELSE
               DISPLAY "NC260A-TEST-1 FAIL"
               DISPLAY "  F1=>" WS-FIELD1 "<"
               DISPLAY "  D1=>" WS-DELIM1 "<"
               DISPLAY "  C1=" WS-COUNT1 " C2=" WS-COUNT2
           END-IF.
       NC260A-TEST-2.
      * UNSTRING with POINTER
      * "XXHELLO::WORLD" with POINTER starting at 3
      * from pos 3: "HELLO::WORLD" delimited by "::"
      *   FIELD1="HELLO", FIELD2="WORLD"
           MOVE "XXHELLO::WORLD" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2.
           MOVE 3 TO WS-PTR.
           UNSTRING WS-SOURCE DELIMITED BY "::"
               INTO WS-FIELD1 WS-FIELD2
               WITH POINTER WS-PTR.
           IF WS-FIELD1 = "HELLO     "
               AND WS-FIELD2 = "WORLD     "
               DISPLAY "NC260A-TEST-2 PASS"
           ELSE
               DISPLAY "NC260A-TEST-2 FAIL"
               DISPLAY "  F1=>" WS-FIELD1 "<"
               DISPLAY "  F2=>" WS-FIELD2 "<"
               DISPLAY "  PTR=" WS-PTR
           END-IF.
       NC260A-TEST-3.
      * UNSTRING with TALLYING
      * "A,B,C" delimited by "," into 3 fields
      * TALLYING should count 3 receiving fields
           MOVE "A,B,C" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-FIELD1 WS-FIELD2 WS-FIELD3
               TALLYING IN WS-TALLY.
           IF WS-TALLY = 3
               AND WS-FIELD1 = "A         "
               AND WS-FIELD2 = "B         "
               AND WS-FIELD3 = "C         "
               DISPLAY "NC260A-TEST-3 PASS"
           ELSE
               DISPLAY "NC260A-TEST-3 FAIL"
               DISPLAY "  TALLY=" WS-TALLY
               DISPLAY "  F1=>" WS-FIELD1 "<"
               DISPLAY "  F2=>" WS-FIELD2 "<"
               DISPLAY "  F3=>" WS-FIELD3 "<"
           END-IF.
