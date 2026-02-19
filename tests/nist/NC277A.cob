       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC277A.
      *
      * NIST CCVS-style test: MOVE numeric to alphanumeric
      * and vice versa (type coercion). Tests how COBOL
      * handles cross-type MOVE operations.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM         PIC 9(5)  VALUE ZEROS.
       01 WS-ALPHA       PIC X(10) VALUE SPACES.
       01 WS-NUM2        PIC 9(3)  VALUE ZEROS.
       01 WS-ALPHA2      PIC X(5)  VALUE SPACES.
       01 WS-NUM3        PIC 9(8)  VALUE ZEROS.
       PROCEDURE DIVISION.
       NC277A-CONTROL.
           PERFORM NC277A-TEST-1.
           PERFORM NC277A-TEST-2.
           PERFORM NC277A-TEST-3.
           STOP RUN.
       NC277A-TEST-1.
      * MOVE numeric to alphanumeric
      *   PIC 9(5) VALUE 42 => "00042"
      *   When moved to PIC X(10), should be "00042     "
      *   (left-justified, space-padded)
           MOVE 42 TO WS-NUM.
           MOVE WS-NUM TO WS-ALPHA.
           IF WS-ALPHA = "00042     "
               DISPLAY "NC277A-TEST-1 PASS"
           ELSE
               DISPLAY "NC277A-TEST-1 FAIL"
               DISPLAY "  Got >" WS-ALPHA "<"
           END-IF.
       NC277A-TEST-2.
      * MOVE alphanumeric digits to numeric
      *   "123" in X(5) => "123  "
      *   Moved to PIC 9(3), should get 123
           MOVE "123" TO WS-ALPHA2.
           MOVE WS-ALPHA2 TO WS-NUM2.
           IF WS-NUM2 = 123
               DISPLAY "NC277A-TEST-2 PASS"
           ELSE
               DISPLAY "NC277A-TEST-2 FAIL"
               DISPLAY "  Expected 123, got " WS-NUM2
           END-IF.
       NC277A-TEST-3.
      * MOVE numeric to alphanumeric preserves digit characters
      *   PIC 9(8) VALUE 99999999 => "99999999"
      *   Moved to PIC X(10) => "99999999  "
           MOVE 99999999 TO WS-NUM3.
           MOVE WS-NUM3 TO WS-ALPHA.
           IF WS-ALPHA = "99999999  "
               DISPLAY "NC277A-TEST-3 PASS"
           ELSE
               DISPLAY "NC277A-TEST-3 FAIL"
               DISPLAY "  Got >" WS-ALPHA "<"
           END-IF.
