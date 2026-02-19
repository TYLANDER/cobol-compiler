       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC174A.
      *
      * NIST CCVS-style test: Alphanumeric comparison
      * Tests IF comparisons between alphanumeric fields
      * using >, <, and = operators. Comparison is by
      * collating sequence (ASCII/EBCDIC order).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A             PIC X(5) VALUE SPACES.
       01 WS-B             PIC X(5) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: "BAKER" > "APPLE" alphabetically
           MOVE "BAKER" TO WS-A.
           MOVE "APPLE" TO WS-B.
           IF WS-A > WS-B
               DISPLAY "NC174A-TEST-1 PASS"
           ELSE
               DISPLAY "NC174A-TEST-1 FAIL"
               DISPLAY "  Expected BAKER > APPLE"
           END-IF.
      * Test 2: "CAT" < "DOG" (shorter strings, space padded)
      *   "CAT  " < "DOG  " in collating sequence
           MOVE "CAT" TO WS-A.
           MOVE "DOG" TO WS-B.
           IF WS-A < WS-B
               DISPLAY "NC174A-TEST-2 PASS"
           ELSE
               DISPLAY "NC174A-TEST-2 FAIL"
               DISPLAY "  Expected CAT < DOG"
           END-IF.
      * Test 3: Equal alphanumeric values
           MOVE "HELLO" TO WS-A.
           MOVE "HELLO" TO WS-B.
           IF WS-A = WS-B
               DISPLAY "NC174A-TEST-3 PASS"
           ELSE
               DISPLAY "NC174A-TEST-3 FAIL"
               DISPLAY "  Expected HELLO = HELLO"
           END-IF.
           STOP RUN.
