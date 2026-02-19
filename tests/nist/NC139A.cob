       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC139A.
      *
      * NIST CCVS-style test: Reference modification
      * Tests substring access using reference modification
      * syntax: identifier(start:length).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR         PIC X(10) VALUE "ABCDEFGHIJ".
       01 WS-SUB1        PIC X(3)  VALUE SPACES.
       01 WS-SUB2        PIC X(2)  VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: WS-VAR(1:3) — first 3 characters
      *   "ABCDEFGHIJ"(1:3) = "ABC"
           MOVE WS-VAR(1:3) TO WS-SUB1.
           IF WS-SUB1 = "ABC"
               DISPLAY "NC139A-TEST-1 PASS"
           ELSE
               DISPLAY "NC139A-TEST-1 FAIL"
               DISPLAY "  Expected ABC, got >" WS-SUB1 "<"
           END-IF.
      * Test 2: WS-VAR(4:2) — 2 chars starting at position 4
      *   "ABCDEFGHIJ"(4:2) = "DE"
           MOVE WS-VAR(4:2) TO WS-SUB2.
           IF WS-SUB2 = "DE"
               DISPLAY "NC139A-TEST-2 PASS"
           ELSE
               DISPLAY "NC139A-TEST-2 FAIL"
               DISPLAY "  Expected DE, got >" WS-SUB2 "<"
           END-IF.
           STOP RUN.
