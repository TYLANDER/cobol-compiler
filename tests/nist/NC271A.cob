       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC271A.
      *
      * NIST CCVS-style test: MOVE figurative constants
      * Tests MOVE SPACES, ZEROS, HIGH-VALUES to different
      * PIC types (alphanumeric, numeric, group).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ALPHA        PIC X(10) VALUE "ABCDEFGHIJ".
       01 WS-NUM          PIC 9(5)  VALUE 12345.
       01 WS-GROUP.
           05 WS-GRP-A    PIC X(5)  VALUE "HELLO".
           05 WS-GRP-N    PIC 9(3)  VALUE 999.
       PROCEDURE DIVISION.
       NC271A-CONTROL.
           PERFORM NC271A-TEST-1.
           PERFORM NC271A-TEST-2.
           PERFORM NC271A-TEST-3.
           STOP RUN.
       NC271A-TEST-1.
      * MOVE SPACES to alphanumeric and ZEROS to numeric
      *   ALPHA should become all spaces
      *   NUM should become 00000
           MOVE SPACES TO WS-ALPHA.
           MOVE ZEROS TO WS-NUM.
           IF WS-ALPHA = SPACES AND WS-NUM = 0
               DISPLAY "NC271A-TEST-1 PASS"
           ELSE
               DISPLAY "NC271A-TEST-1 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA "<"
               DISPLAY "  NUM=" WS-NUM
           END-IF.
       NC271A-TEST-2.
      * MOVE ZEROS to alphanumeric field
      *   PIC X(10) filled with character zeros "0000000000"
           MOVE ZEROS TO WS-ALPHA.
           IF WS-ALPHA = "0000000000"
               DISPLAY "NC271A-TEST-2 PASS"
           ELSE
               DISPLAY "NC271A-TEST-2 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA "<"
           END-IF.
       NC271A-TEST-3.
      * MOVE HIGH-VALUES to alphanumeric field
      *   HIGH-VALUES fills with highest character in collating
      *   sequence; should NOT equal SPACES or ZEROS
           MOVE HIGH-VALUES TO WS-ALPHA.
           IF WS-ALPHA NOT = SPACES
               AND WS-ALPHA NOT = ZEROS
               DISPLAY "NC271A-TEST-3 PASS"
           ELSE
               DISPLAY "NC271A-TEST-3 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA "<"
           END-IF.
