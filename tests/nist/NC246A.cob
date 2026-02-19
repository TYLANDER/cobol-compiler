       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC246A.
      *
      * NIST CCVS-style test: DISPLAY with NO ADVANCING
      * Tests that NO ADVANCING keeps cursor on same line.
      * Two NO ADVANCING displays followed by a normal display
      * should produce one complete line.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PART1    PIC X(10) VALUE "HELLO".
       01 WS-PART2    PIC X(10) VALUE "WORLD".
       PROCEDURE DIVISION.
      * Test 1: Basic NO ADVANCING display
      *   Two parts on one line followed by normal display
           DISPLAY "NC246A-TEST-1 "
               WITH NO ADVANCING.
           DISPLAY "PASS".
      * Test 2: Multiple NO ADVANCING followed by newline
           DISPLAY "NC246A"
               WITH NO ADVANCING.
           DISPLAY "-TEST-2 "
               WITH NO ADVANCING.
           DISPLAY "PASS".
      * Test 3: NO ADVANCING with variables
           DISPLAY "NC246A-TEST-3 "
               WITH NO ADVANCING.
           MOVE "PASS" TO WS-PART1.
           DISPLAY WS-PART1.
           STOP RUN.
