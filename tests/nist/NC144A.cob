       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC144A.
      *
      * NIST CCVS-style test: UNSTRING with COUNT and TALLYING
      * Tests the UNSTRING statement to split a delimited string
      * into receiving fields with COUNT IN and TALLYING IN.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE       PIC X(30) VALUE SPACES.
       01 WS-FIELD1       PIC X(10) VALUE SPACES.
       01 WS-FIELD2       PIC X(10) VALUE SPACES.
       01 WS-FIELD3       PIC X(10) VALUE SPACES.
       01 WS-CNT1         PIC 9(4)  VALUE ZEROS.
       01 WS-CNT2         PIC 9(4)  VALUE ZEROS.
       01 WS-CNT3         PIC 9(4)  VALUE ZEROS.
       01 WS-TALLY        PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: UNSTRING with DELIMITED BY and COUNT IN
      *   Split "ABC,DE,FGHIJ" by comma
      *   FIELD1 = "ABC" (count 3), FIELD2 = "DE" (count 2),
      *   FIELD3 = "FGHIJ" (count 5)
           MOVE "ABC,DE,FGHIJ" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           MOVE 0 TO WS-CNT1 WS-CNT2 WS-CNT3.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-FIELD1 COUNT IN WS-CNT1
                    WS-FIELD2 COUNT IN WS-CNT2
                    WS-FIELD3 COUNT IN WS-CNT3.
           IF WS-FIELD1(1:3) = "ABC"
               AND WS-CNT1 = 3
               AND WS-FIELD2(1:2) = "DE"
               AND WS-CNT2 = 2
               AND WS-FIELD3(1:5) = "FGHIJ"
               AND WS-CNT3 = 23
               DISPLAY "NC144A-TEST-1 PASS"
           ELSE
               DISPLAY "NC144A-TEST-1 FAIL"
               DISPLAY "  F1=>" WS-FIELD1 "< C1=" WS-CNT1
               DISPLAY "  F2=>" WS-FIELD2 "< C2=" WS-CNT2
               DISPLAY "  F3=>" WS-FIELD3 "< C3=" WS-CNT3
           END-IF.
      * Test 2: UNSTRING with TALLYING IN
      *   Count how many receiving fields were acted upon
      *   "X;Y;Z" split by ";" into 3 fields => TALLY = 3
           MOVE "X;Y;Z" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SOURCE DELIMITED BY ";"
               INTO WS-FIELD1
                    WS-FIELD2
                    WS-FIELD3
               TALLYING IN WS-TALLY.
           IF WS-TALLY = 3
               DISPLAY "NC144A-TEST-2 PASS"
           ELSE
               DISPLAY "NC144A-TEST-2 FAIL"
               DISPLAY "  Expected TALLY=3, got " WS-TALLY
           END-IF.
           STOP RUN.
