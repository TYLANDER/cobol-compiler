       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC104A.
      *
      * NIST CCVS-style test: MULTIPLY statement
      * Tests MULTIPLY BY and MULTIPLY BY GIVING.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC 9(4) VALUE ZEROS.
       01 WS-B PIC 9(4) VALUE ZEROS.
       01 WS-RESULT PIC 9(8) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: MULTIPLY A BY B GIVING RESULT
           MOVE 0012 TO WS-A.
           MOVE 0025 TO WS-B.
           MULTIPLY WS-A BY WS-B GIVING WS-RESULT.
           IF WS-RESULT = 300
               DISPLAY "NC104A-TEST-1 PASS"
           ELSE
               DISPLAY "NC104A-TEST-1 FAIL"
           END-IF.
      * Test 2: MULTIPLY with larger values
           MOVE 1234 TO WS-A.
           MOVE 0005 TO WS-B.
           MULTIPLY WS-A BY WS-B GIVING WS-RESULT.
           IF WS-RESULT = 6170
               DISPLAY "NC104A-TEST-2 PASS"
           ELSE
               DISPLAY "NC104A-TEST-2 FAIL"
           END-IF.
      * Test 3: MULTIPLY by 1 (identity)
           MOVE 9999 TO WS-A.
           MOVE 0001 TO WS-B.
           MULTIPLY WS-A BY WS-B GIVING WS-RESULT.
           IF WS-RESULT = 9999
               DISPLAY "NC104A-TEST-3 PASS"
           ELSE
               DISPLAY "NC104A-TEST-3 FAIL"
           END-IF.
      * Test 4: MULTIPLY by 0
           MOVE 5000 TO WS-A.
           MOVE 0000 TO WS-B.
           MULTIPLY WS-A BY WS-B GIVING WS-RESULT.
           IF WS-RESULT = 0
               DISPLAY "NC104A-TEST-4 PASS"
           ELSE
               DISPLAY "NC104A-TEST-4 FAIL"
           END-IF.
           STOP RUN.
