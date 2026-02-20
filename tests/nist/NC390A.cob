       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC390A.
      *
      * NIST CCVS-style test: MOVE with sign handling
      * Tests signed arithmetic and positive signed to unsigned.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SIGNED          PIC S9(3) VALUE 0.
       01 WS-UNSIGNED         PIC 9(3) VALUE 0.
       01 WS-SIGNED2         PIC S9(3) VALUE 0.
       01 WS-RESULT          PIC S9(5) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Move positive signed to unsigned
           MOVE 42 TO WS-SIGNED.
           MOVE WS-SIGNED TO WS-UNSIGNED.
           IF WS-UNSIGNED = 42
               DISPLAY "NC390A-TEST-1 PASS"
           ELSE
               DISPLAY "NC390A-TEST-1 FAIL"
               DISPLAY "  Expected 42, got " WS-UNSIGNED
           END-IF.
      * Test 2: Move positive to signed preserves value
           MOVE 99 TO WS-SIGNED.
           MOVE WS-SIGNED TO WS-SIGNED2.
           IF WS-SIGNED2 = 99
               DISPLAY "NC390A-TEST-2 PASS"
           ELSE
               DISPLAY "NC390A-TEST-2 FAIL"
               DISPLAY "  Expected 99, got " WS-SIGNED2
           END-IF.
      * Test 3: Arithmetic with negative signed values
           MOVE -10 TO WS-SIGNED.
           MOVE -20 TO WS-SIGNED2.
           ADD WS-SIGNED TO WS-SIGNED2
               GIVING WS-RESULT.
           IF WS-RESULT = -30
               DISPLAY "NC390A-TEST-3 PASS"
           ELSE
               DISPLAY "NC390A-TEST-3 FAIL"
               DISPLAY "  Expected -30, got " WS-RESULT
           END-IF.
           STOP RUN.
