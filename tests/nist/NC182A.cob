       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC182A.
      *
      * NIST CCVS-style test: MOVE category conversions
      * Tests MOVE numeric to alphanumeric and MOVE alphanumeric
      * to numeric, verifying data category conversion rules.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM            PIC 9(5) VALUE ZEROS.
       01 WS-ALPHA          PIC X(10) VALUE SPACES.
       01 WS-NUM2           PIC 9(3) VALUE ZEROS.
       01 WS-EDITED         PIC Z(4)9 VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: MOVE numeric to alphanumeric
      *   MOVE 12345 to PIC X(10) => right-justified digits
      *   as alphanumeric left-justified "12345     "
           MOVE 12345 TO WS-NUM.
           MOVE WS-NUM TO WS-ALPHA.
           IF WS-ALPHA(1:5) = "12345"
               DISPLAY "NC182A-TEST-1 PASS"
           ELSE
               DISPLAY "NC182A-TEST-1 FAIL"
               DISPLAY "  Expected '12345', got >"
                   WS-ALPHA "<"
           END-IF.
      * Test 2: MOVE alphanumeric numeric string to numeric
      *   MOVE "789" stored in PIC X to PIC 9(3)
           MOVE "789" TO WS-ALPHA.
           MOVE WS-ALPHA(1:3) TO WS-NUM2.
           IF WS-NUM2 = 789
               DISPLAY "NC182A-TEST-2 PASS"
           ELSE
               DISPLAY "NC182A-TEST-2 FAIL"
               DISPLAY "  Expected 789, got " WS-NUM2
           END-IF.
      * Test 3: MOVE numeric to numeric-edited
      *   MOVE 42 to PIC Z(4)9 => "   42"
           MOVE 42 TO WS-NUM.
           MOVE WS-NUM TO WS-EDITED.
           IF WS-EDITED = "   42"
               DISPLAY "NC182A-TEST-3 PASS"
           ELSE
               DISPLAY "NC182A-TEST-3 FAIL"
               DISPLAY "  Expected '   42', got >"
                   WS-EDITED "<"
           END-IF.
           STOP RUN.
