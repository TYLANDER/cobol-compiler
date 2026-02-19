       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC226A.
      *
      * NIST CCVS-style test: UNSTRING with ALL delimiter
      * Tests UNSTRING using ALL delimiter to collapse multiple
      * consecutive delimiters into one logical separator.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC1          PIC X(20) VALUE SPACES.
       01 WS-SRC2          PIC X(20) VALUE SPACES.
       01 WS-FIELD1        PIC X(10) VALUE SPACES.
       01 WS-FIELD2        PIC X(10) VALUE SPACES.
       01 WS-FIELD3        PIC X(10) VALUE SPACES.
       01 WS-TALLY         PIC 9(2) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: UNSTRING with ALL "," delimiter
      *   "A,,B" delimited by ALL "," gives "A" and "B"
      *   The consecutive commas are treated as one delimiter
           MOVE "A,,B" TO WS-SRC1.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SRC1 DELIMITED BY ALL ","
               INTO WS-FIELD1
                    WS-FIELD2
               TALLYING IN WS-TALLY.
           IF WS-FIELD1(1:1) = "A"
               AND WS-FIELD2(1:1) = "B"
               AND WS-TALLY = 2
               DISPLAY "NC226A-TEST-1 PASS"
           ELSE
               DISPLAY "NC226A-TEST-1 FAIL"
               DISPLAY "  F1=>" WS-FIELD1 "<"
               DISPLAY "  F2=>" WS-FIELD2 "<"
               DISPLAY "  TALLY=" WS-TALLY
           END-IF.
      * Test 2: UNSTRING with ALL ";" and three fields
      *   "X;;;Y;;;Z" with ALL ";" => "X", "Y", "Z"
           MOVE "X;;;Y;;;Z" TO WS-SRC2.
           MOVE SPACES TO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           MOVE 0 TO WS-TALLY.
           UNSTRING WS-SRC2 DELIMITED BY ALL ";"
               INTO WS-FIELD1
                    WS-FIELD2
                    WS-FIELD3
               TALLYING IN WS-TALLY.
           IF WS-FIELD1(1:1) = "X"
               AND WS-FIELD2(1:1) = "Y"
               AND WS-FIELD3(1:1) = "Z"
               AND WS-TALLY = 3
               DISPLAY "NC226A-TEST-2 PASS"
           ELSE
               DISPLAY "NC226A-TEST-2 FAIL"
               DISPLAY "  F1=>" WS-FIELD1 "<"
               DISPLAY "  F2=>" WS-FIELD2 "<"
               DISPLAY "  F3=>" WS-FIELD3 "<"
               DISPLAY "  TALLY=" WS-TALLY
           END-IF.
           STOP RUN.
