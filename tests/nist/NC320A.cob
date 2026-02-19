       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC320A.
      *
      * NIST CCVS-style test: MOVE figurative constants
      * Tests MOVE ZEROS, SPACES, HIGH-VALUES, LOW-VALUES,
      * and QUOTES to alphanumeric fields.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIELD        PIC X(5) VALUE "ABCDE".
       01 WS-NUM          PIC 9(4) VALUE 1234.
       01 WS-CHECK        PIC X(5) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: MOVE ZEROS and SPACES to their respective types
      *   ZEROS to PIC 9 => 0000
      *   SPACES to PIC X => all spaces
           MOVE ZEROS TO WS-NUM.
           MOVE SPACES TO WS-FIELD.
           IF WS-NUM = 0 AND WS-FIELD = SPACES
               DISPLAY "NC320A-TEST-1 PASS"
           ELSE
               DISPLAY "NC320A-TEST-1 FAIL"
               DISPLAY "  NUM=" WS-NUM
               DISPLAY "  FIELD=>" WS-FIELD "<"
           END-IF.
      * Test 2: MOVE HIGH-VALUES then LOW-VALUES
      *   HIGH-VALUES fills with max char (xFF)
      *   LOW-VALUES fills with min char (x00)
      *   They should not be equal to each other
           MOVE HIGH-VALUES TO WS-FIELD.
           MOVE LOW-VALUES TO WS-CHECK.
           IF WS-FIELD NOT = WS-CHECK
               AND WS-FIELD NOT = SPACES
               AND WS-CHECK NOT = SPACES
               DISPLAY "NC320A-TEST-2 PASS"
           ELSE
               DISPLAY "NC320A-TEST-2 FAIL"
               DISPLAY "  HIGH and LOW should differ"
           END-IF.
      * Test 3: MOVE ZEROS to alphanumeric fills with "0"
      *   PIC X(5) gets "00000"
           MOVE ZEROS TO WS-FIELD.
           IF WS-FIELD = "00000"
               DISPLAY "NC320A-TEST-3 PASS"
           ELSE
               DISPLAY "NC320A-TEST-3 FAIL"
               DISPLAY "  Expected >00000<, got >"
                   WS-FIELD "<"
           END-IF.
           STOP RUN.
