       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC396A.
      *
      * NIST CCVS-style test: MOVE SPACES/ZEROS to various types
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ALPHA           PIC X(5) VALUE "HELLO".
       01 WS-NUM             PIC 9(5) VALUE 12345.
       01 WS-ALPHA2          PIC X(5) VALUE "WORLD".
       PROCEDURE DIVISION.
      * Test 1: MOVE SPACES to alphanumeric
           MOVE SPACES TO WS-ALPHA.
           IF WS-ALPHA = "     "
               DISPLAY "NC396A-TEST-1 PASS"
           ELSE
               DISPLAY "NC396A-TEST-1 FAIL"
               DISPLAY "  Got [" WS-ALPHA "]"
           END-IF.
      * Test 2: MOVE ZEROS to numeric
           MOVE ZEROS TO WS-NUM.
           IF WS-NUM = 0
               DISPLAY "NC396A-TEST-2 PASS"
           ELSE
               DISPLAY "NC396A-TEST-2 FAIL"
               DISPLAY "  Got " WS-NUM
           END-IF.
      * Test 3: MOVE ZEROS to alphanumeric (fills with "0")
           MOVE ZEROS TO WS-ALPHA2.
           IF WS-ALPHA2 = "00000"
               DISPLAY "NC396A-TEST-3 PASS"
           ELSE
               DISPLAY "NC396A-TEST-3 FAIL"
               DISPLAY "  Got [" WS-ALPHA2 "]"
           END-IF.
           STOP RUN.
