       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC172A.
      *
      * NIST CCVS-style test: BLANK WHEN ZERO clause
      * Tests that a numeric display field shows spaces
      * when the value is zero.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-AMT1          PIC 9(4) BLANK WHEN ZERO.
       01 WS-AMT2          PIC 9(4) BLANK WHEN ZERO.
       01 WS-AMT3          PIC Z(3)9 BLANK WHEN ZERO.
       PROCEDURE DIVISION.
      * Test 1: Zero value => all spaces
      *   PIC 9(4) BLANK WHEN ZERO with value 0 => "    "
           MOVE 0 TO WS-AMT1.
           IF WS-AMT1 = SPACES
               DISPLAY "NC172A-TEST-1 PASS"
           ELSE
               DISPLAY "NC172A-TEST-1 FAIL"
               DISPLAY "  Expected spaces got ["
                   WS-AMT1 "]"
           END-IF.
      * Test 2: Non-zero value => normal display
      *   PIC 9(4) BLANK WHEN ZERO with value 42 => "0042"
           MOVE 42 TO WS-AMT2.
           IF WS-AMT2 = "0042"
               DISPLAY "NC172A-TEST-2 PASS"
           ELSE
               DISPLAY "NC172A-TEST-2 FAIL"
               DISPLAY "  Expected [0042] got ["
                   WS-AMT2 "]"
           END-IF.
      * Test 3: Edited field with BLANK WHEN ZERO
      *   PIC Z(3)9 BLANK WHEN ZERO with value 0 => "    "
           MOVE 0 TO WS-AMT3.
           IF WS-AMT3 = SPACES
               DISPLAY "NC172A-TEST-3 PASS"
           ELSE
               DISPLAY "NC172A-TEST-3 FAIL"
               DISPLAY "  Expected spaces got ["
                   WS-AMT3 "]"
           END-IF.
           STOP RUN.
