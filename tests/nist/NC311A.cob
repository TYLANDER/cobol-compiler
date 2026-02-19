       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC311A.
      *
      * NIST CCVS-style test: MOVE with numeric truncation
      * Tests that moving a large value to a smaller PIC field
      * truncates the high-order digits as per COBOL rules.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-BIG          PIC 9(5) VALUE ZEROS.
       01 WS-SMALL         PIC 9(2) VALUE ZEROS.
       01 WS-MED           PIC 9(3) VALUE ZEROS.
       01 WS-TINY          PIC 9(1) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: MOVE 5-digit value to PIC 9(2)
      *   98765 moved to PIC 9(2) => truncated to 65
           MOVE 98765 TO WS-BIG.
           MOVE WS-BIG TO WS-SMALL.
           IF WS-SMALL = 65
               DISPLAY "NC311A-TEST-1 PASS"
           ELSE
               DISPLAY "NC311A-TEST-1 FAIL"
               DISPLAY "  Expected 65, got " WS-SMALL
           END-IF.
      * Test 2: MOVE 5-digit value to PIC 9(3)
      *   54321 moved to PIC 9(3) => truncated to 321
           MOVE 54321 TO WS-BIG.
           MOVE WS-BIG TO WS-MED.
           IF WS-MED = 321
               DISPLAY "NC311A-TEST-2 PASS"
           ELSE
               DISPLAY "NC311A-TEST-2 FAIL"
               DISPLAY "  Expected 321, got " WS-MED
           END-IF.
      * Test 3: MOVE 5-digit value to PIC 9(1)
      *   10007 moved to PIC 9(1) => truncated to 7
           MOVE 10007 TO WS-BIG.
           MOVE WS-BIG TO WS-TINY.
           IF WS-TINY = 7
               DISPLAY "NC311A-TEST-3 PASS"
           ELSE
               DISPLAY "NC311A-TEST-3 FAIL"
               DISPLAY "  Expected 7, got " WS-TINY
           END-IF.
           STOP RUN.
