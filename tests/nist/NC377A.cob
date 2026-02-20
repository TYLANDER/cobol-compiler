       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC377A.
      *
      * NIST CCVS-style test: INSPECT TALLYING with LEADING
      * and TRAILING.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIELD1       PIC X(10) VALUE "000ABC0000".
       01 WS-FIELD2       PIC X(10) VALUE "ABC   DEF ".
       01 WS-FIELD3       PIC X(12) VALUE "00HELLO00   ".
       01 WS-TALLY        PIC 9(3) VALUE 0.
       01 WS-TALLY2       PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: INSPECT TALLYING FOR LEADING "0"
      * "000ABC0000" has 3 leading zeros
           MOVE 0 TO WS-TALLY.
           INSPECT WS-FIELD1 TALLYING WS-TALLY
               FOR LEADING "0".
           IF WS-TALLY = 3
               DISPLAY "NC377A-TEST-1 PASS"
           ELSE
               DISPLAY "NC377A-TEST-1 FAIL"
               DISPLAY "  Expected 3, got " WS-TALLY
           END-IF.
      * Test 2: INSPECT TALLYING FOR TRAILING SPACES
      * "ABC   DEF " has 1 trailing space
           MOVE 0 TO WS-TALLY.
           INSPECT WS-FIELD2 TALLYING WS-TALLY
               FOR TRAILING " ".
           IF WS-TALLY = 1
               DISPLAY "NC377A-TEST-2 PASS"
           ELSE
               DISPLAY "NC377A-TEST-2 FAIL"
               DISPLAY "  Expected 1, got " WS-TALLY
           END-IF.
      * Test 3: INSPECT with LEADING and TRAILING (separate)
      * "00HELLO00   " => LEADING "0" = 2, TRAILING " " = 3
           MOVE 0 TO WS-TALLY.
           INSPECT WS-FIELD3 TALLYING WS-TALLY
               FOR LEADING "0".
           MOVE 0 TO WS-TALLY2.
           INSPECT WS-FIELD3 TALLYING WS-TALLY2
               FOR TRAILING " ".
           IF WS-TALLY = 2 AND WS-TALLY2 = 3
               DISPLAY "NC377A-TEST-3 PASS"
           ELSE
               DISPLAY "NC377A-TEST-3 FAIL"
               DISPLAY "  Expected LEAD=2, got " WS-TALLY
               DISPLAY "  Expected TRAIL=3, got " WS-TALLY2
           END-IF.
           STOP RUN.
