       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC342A.
      *
      * NIST CCVS-style test: UNSTRING with TALLYING IN
      * Tests that TALLYING IN counts the number of receiving
      * fields that are populated by the UNSTRING operation.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE       PIC X(30) VALUE SPACES.
       01 WS-FIELD1       PIC X(10) VALUE SPACES.
       01 WS-FIELD2       PIC X(10) VALUE SPACES.
       01 WS-FIELD3       PIC X(10) VALUE SPACES.
       01 WS-PTR          PIC 99 VALUE 1.
       01 WS-TALLY        PIC 99 VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: UNSTRING into 3 fields, all populated
      *   "AA,BB,CC" => 3 fields => TALLY = 3
           MOVE "AA,BB,CC" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-FIELD1 WS-FIELD2 WS-FIELD3
               WITH POINTER WS-PTR
               TALLYING IN WS-TALLY.
           IF WS-TALLY = 3
               AND WS-FIELD1(1:2) = "AA"
               AND WS-FIELD2(1:2) = "BB"
               AND WS-FIELD3(1:2) = "CC"
               DISPLAY "NC342A-TEST-1 PASS"
           ELSE
               DISPLAY "NC342A-TEST-1 FAIL"
               DISPLAY "  TALLY=" WS-TALLY
               DISPLAY "  F1=>" WS-FIELD1 "<"
               DISPLAY "  F2=>" WS-FIELD2 "<"
               DISPLAY "  F3=>" WS-FIELD3 "<"
           END-IF.
      * Test 2: UNSTRING into 3 fields, only 2 populated
      *   "XX,YY" => 2 fields => TALLY = 2
           MOVE "XX,YY" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-FIELD1 WS-FIELD2 WS-FIELD3
               WITH POINTER WS-PTR
               TALLYING IN WS-TALLY.
           IF WS-TALLY = 2
               DISPLAY "NC342A-TEST-2 PASS"
           ELSE
               DISPLAY "NC342A-TEST-2 FAIL"
               DISPLAY "  Expected TALLY=2, got " WS-TALLY
           END-IF.
      * Test 3: UNSTRING single field, no delimiter found
      *   "HELLO" with delimiter "," => 1 field => TALLY = 1
           MOVE "HELLO" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2.
           MOVE 1 TO WS-PTR.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-FIELD1 WS-FIELD2
               WITH POINTER WS-PTR
               TALLYING IN WS-TALLY.
           IF WS-TALLY = 1
               AND WS-FIELD1(1:5) = "HELLO"
               DISPLAY "NC342A-TEST-3 PASS"
           ELSE
               DISPLAY "NC342A-TEST-3 FAIL"
               DISPLAY "  TALLY=" WS-TALLY
               DISPLAY "  F1=>" WS-FIELD1 "<"
           END-IF.
           STOP RUN.
