       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC293A.
      *
      * NIST CCVS-style test: BLANK WHEN ZERO clause
      * Tests that fields with BLANK WHEN ZERO display spaces
      * when value is zero, and normal digits when non-zero.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-BWZ1        PIC 9(4) BLANK WHEN ZERO.
       01 WS-BWZ2        PIC 9(4) BLANK WHEN ZERO.
       01 WS-BWZ3        PIC 9(6) BLANK WHEN ZERO.
       PROCEDURE DIVISION.
      * Test 1: Move 0 to BLANK WHEN ZERO field => spaces
      *   PIC 9(4) BLANK WHEN ZERO, value 0 => "    "
           MOVE 0 TO WS-BWZ1.
           IF WS-BWZ1 = SPACES
               DISPLAY "NC293A-TEST-1 PASS"
           ELSE
               DISPLAY "NC293A-TEST-1 FAIL"
               DISPLAY "  Expected spaces got [" WS-BWZ1 "]"
           END-IF.
      * Test 2: Non-zero in BLANK WHEN ZERO field => digits
      *   PIC 9(4) BLANK WHEN ZERO, value 15 => "0015"
           MOVE 15 TO WS-BWZ2.
           IF WS-BWZ2 = "0015"
               DISPLAY "NC293A-TEST-2 PASS"
           ELSE
               DISPLAY "NC293A-TEST-2 FAIL"
               DISPLAY "  Expected [0015] got [" WS-BWZ2 "]"
           END-IF.
      * Test 3: Larger BLANK WHEN ZERO field with zero => spaces
      *   PIC 9(6) BLANK WHEN ZERO, value 0 => "      "
           MOVE 0 TO WS-BWZ3.
           IF WS-BWZ3 = SPACES
               DISPLAY "NC293A-TEST-3 PASS"
           ELSE
               DISPLAY "NC293A-TEST-3 FAIL"
               DISPLAY "  Expected spaces got [" WS-BWZ3 "]"
           END-IF.
           STOP RUN.
