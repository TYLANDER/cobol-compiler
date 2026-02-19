       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC317A.
      *
      * NIST CCVS-style test: UNSTRING with WITH POINTER clause
      * Tests that UNSTRING starts parsing from the position
      * indicated by the POINTER variable.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE       PIC X(30) VALUE SPACES.
       01 WS-FIELD1       PIC X(10) VALUE SPACES.
       01 WS-FIELD2       PIC X(10) VALUE SPACES.
       01 WS-FIELD3       PIC X(10) VALUE SPACES.
       01 WS-PTR          PIC 99 VALUE 1.
       01 WS-TALLY        PIC 99 VALUE ZEROS.
       PROCEDURE DIVISION.
       NC317A-CONTROL.
           PERFORM NC317A-TEST-1.
           PERFORM NC317A-TEST-2.
           PERFORM NC317A-TEST-3.
           STOP RUN.
       NC317A-TEST-1.
      * UNSTRING with POINTER starting at position 4
      *   "XXXHELLO,WORLD" from pos 4 => "HELLO" and "WORLD"
           MOVE "XXXHELLO,WORLD" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2.
           MOVE 4 TO WS-PTR.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-FIELD1 WS-FIELD2
               WITH POINTER WS-PTR.
           IF WS-FIELD1(1:5) = "HELLO"
               AND WS-FIELD2(1:5) = "WORLD"
               DISPLAY "NC317A-TEST-1 PASS"
           ELSE
               DISPLAY "NC317A-TEST-1 FAIL"
               DISPLAY "  F1=>" WS-FIELD1 "<"
               DISPLAY "  F2=>" WS-FIELD2 "<"
           END-IF.
       NC317A-TEST-2.
      * POINTER value updated after UNSTRING
      *   "XXXHELLO,WORLD" in PIC X(30), POINTER starts at 4
      *   With 2 INTO fields, all remaining data is consumed
      *   PTR should point past last char in source = 31
           MOVE "XXXHELLO,WORLD" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2.
           MOVE 4 TO WS-PTR.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-FIELD1 WS-FIELD2
               WITH POINTER WS-PTR.
           IF WS-PTR = 31
               DISPLAY "NC317A-TEST-2 PASS"
           ELSE
               DISPLAY "NC317A-TEST-2 FAIL"
               DISPLAY "  Expected PTR=31, got " WS-PTR
           END-IF.
       NC317A-TEST-3.
      * UNSTRING with POINTER at 1 behaves normally
      *   "AA;BB;CC" from pos 1 => "AA", "BB", "CC"
           MOVE "AA;BB;CC" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SOURCE DELIMITED BY ";"
               INTO WS-FIELD1 WS-FIELD2 WS-FIELD3
               WITH POINTER WS-PTR
               TALLYING IN WS-TALLY.
           IF WS-FIELD1(1:2) = "AA"
               AND WS-FIELD2(1:2) = "BB"
               AND WS-FIELD3(1:2) = "CC"
               AND WS-TALLY = 3
               DISPLAY "NC317A-TEST-3 PASS"
           ELSE
               DISPLAY "NC317A-TEST-3 FAIL"
               DISPLAY "  F1=>" WS-FIELD1 "<"
               DISPLAY "  F2=>" WS-FIELD2 "<"
               DISPLAY "  F3=>" WS-FIELD3 "<"
               DISPLAY "  TALLY=" WS-TALLY
           END-IF.
