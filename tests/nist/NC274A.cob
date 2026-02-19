       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC274A.
      *
      * NIST CCVS-style test: UNSTRING with TALLYING
      * Tests UNSTRING with TALLYING phrase using single delimiters.
      * (COUNT IN and DELIMITER IN are not yet supported;
      *  multiple delimiters via OR are not yet supported.)
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE     PIC X(30) VALUE SPACES.
       01 WS-F1         PIC X(10) VALUE SPACES.
       01 WS-F2         PIC X(10) VALUE SPACES.
       01 WS-F3         PIC X(10) VALUE SPACES.
       01 WS-TALLY      PIC 9(2)  VALUE ZEROS.
       PROCEDURE DIVISION.
       NC274A-CONTROL.
           PERFORM NC274A-TEST-1.
           PERFORM NC274A-TEST-2.
           PERFORM NC274A-TEST-3.
           STOP RUN.
       NC274A-TEST-1.
      * UNSTRING with TALLYING - multi-char delimiter
      * "ONE::TWO::THREE" delimited by "::"
      *   F1="ONE" F2="TWO" F3="THREE"
      *   TALLY=3
           MOVE "ONE::TWO::THREE" TO WS-SOURCE.
           MOVE SPACES TO WS-F1 WS-F2 WS-F3.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SOURCE DELIMITED BY "::"
               INTO WS-F1
                    WS-F2
                    WS-F3
               TALLYING IN WS-TALLY.
           IF WS-TALLY = 3
               AND WS-F1 = "ONE       "
               AND WS-F2 = "TWO       "
               AND WS-F3 = "THREE     "
               DISPLAY "NC274A-TEST-1 PASS"
           ELSE
               DISPLAY "NC274A-TEST-1 FAIL"
               DISPLAY "  TALLY=" WS-TALLY
               DISPLAY "  F1=>" WS-F1 "<"
               DISPLAY "  F2=>" WS-F2 "<"
               DISPLAY "  F3=>" WS-F3 "<"
           END-IF.
       NC274A-TEST-2.
      * UNSTRING with TALLYING - comma delimiter
      * "APPLE,BANANA,CHERRY" delimited by ","
      *   F1="APPLE" F2="BANANA" F3="CHERRY"
      *   TALLY=3
           MOVE "APPLE,BANANA,CHERRY" TO WS-SOURCE.
           MOVE SPACES TO WS-F1 WS-F2 WS-F3.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-F1
                    WS-F2
                    WS-F3
               TALLYING IN WS-TALLY.
           IF WS-TALLY = 3
               AND WS-F1 = "APPLE     "
               AND WS-F2 = "BANANA    "
               AND WS-F3 = "CHERRY    "
               DISPLAY "NC274A-TEST-2 PASS"
           ELSE
               DISPLAY "NC274A-TEST-2 FAIL"
               DISPLAY "  TALLY=" WS-TALLY
               DISPLAY "  F1=>" WS-F1 "<"
               DISPLAY "  F2=>" WS-F2 "<"
               DISPLAY "  F3=>" WS-F3 "<"
           END-IF.
       NC274A-TEST-3.
      * UNSTRING with only 2 receiving fields for 3 tokens
      * "X,Y,Z" delimited by "," into 2 fields
      *   F1="X" F2="Y" TALLY=2
           MOVE "X,Y,Z" TO WS-SOURCE.
           MOVE SPACES TO WS-F1 WS-F2.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-F1
                    WS-F2
               TALLYING IN WS-TALLY.
           IF WS-TALLY = 2
               AND WS-F1 = "X         "
               AND WS-F2 = "Y         "
               DISPLAY "NC274A-TEST-3 PASS"
           ELSE
               DISPLAY "NC274A-TEST-3 FAIL"
               DISPLAY "  TALLY=" WS-TALLY
               DISPLAY "  F1=>" WS-F1 "<"
               DISPLAY "  F2=>" WS-F2 "<"
           END-IF.
