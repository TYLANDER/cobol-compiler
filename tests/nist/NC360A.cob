       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC360A.
      *
      * NIST CCVS-style test: MOVE SPACES/ZEROS to group items.
      * Tests figurative constants with group-level MOVE targets.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP-1.
          05 WS-FIELD-1A    PIC X(5).
          05 WS-FIELD-1B    PIC X(5).
       01 WS-GROUP-2.
          05 WS-FIELD-2A    PIC X(5).
          05 WS-FIELD-2B    PIC X(5).
       01 WS-FIELD-3A       PIC X(5) VALUE SPACES.
       01 WS-FIELD-3B       PIC X(5) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: MOVE SPACES to group item
      *   Both subordinate fields should contain spaces.
           MOVE "XXXXX" TO WS-FIELD-1A.
           MOVE "YYYYY" TO WS-FIELD-1B.
           MOVE SPACES TO WS-GROUP-1.
           IF WS-FIELD-1A = SPACES
               AND WS-FIELD-1B = SPACES
               DISPLAY "NC360A-TEST-1 PASS"
           ELSE
               DISPLAY "NC360A-TEST-1 FAIL"
               DISPLAY "  Fields should be SPACES"
           END-IF.
      * Test 2: MOVE ZEROS to group item
      *   Both subordinate fields should contain "00000".
           MOVE "AAAAA" TO WS-FIELD-2A.
           MOVE "BBBBB" TO WS-FIELD-2B.
           MOVE ZEROS TO WS-GROUP-2.
           IF WS-FIELD-2A = "00000"
               AND WS-FIELD-2B = "00000"
               DISPLAY "NC360A-TEST-2 PASS"
           ELSE
               DISPLAY "NC360A-TEST-2 FAIL"
               DISPLAY "  Fields should be '00000'"
           END-IF.
      * Test 3: MOVE figurative constants to elementary items
      *   Move SPACES then ZEROS to verify both work on
      *   elementary fields.
           MOVE ZEROS TO WS-FIELD-3A.
           IF WS-FIELD-3A = "00000"
               MOVE SPACES TO WS-FIELD-3A
               IF WS-FIELD-3A = SPACES
                   DISPLAY "NC360A-TEST-3 PASS"
               ELSE
                   DISPLAY "NC360A-TEST-3 FAIL"
                   DISPLAY "  SPACES MOVE failed"
               END-IF
           ELSE
               DISPLAY "NC360A-TEST-3 FAIL"
               DISPLAY "  ZEROS MOVE failed"
           END-IF.
           STOP RUN.
