       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC105A.
      *
      * NIST CCVS-style test: DIVIDE statement
      * Tests DIVIDE BY GIVING forms.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC 9(4) VALUE ZEROS.
       01 WS-B PIC 9(4) VALUE ZEROS.
       01 WS-RESULT PIC 9(8) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: DIVIDE A BY B GIVING RESULT
           MOVE 0100 TO WS-A.
           MOVE 0005 TO WS-B.
           DIVIDE WS-A BY WS-B GIVING WS-RESULT.
           IF WS-RESULT = 20
               DISPLAY "NC105A-TEST-1 PASS"
           ELSE
               DISPLAY "NC105A-TEST-1 FAIL"
           END-IF.
      * Test 2: DIVIDE with exact division
           MOVE 9999 TO WS-A.
           MOVE 0003 TO WS-B.
           DIVIDE WS-A BY WS-B GIVING WS-RESULT.
           IF WS-RESULT = 3333
               DISPLAY "NC105A-TEST-2 PASS"
           ELSE
               DISPLAY "NC105A-TEST-2 FAIL"
           END-IF.
      * Test 3: DIVIDE by 1 (identity)
           MOVE 4567 TO WS-A.
           MOVE 0001 TO WS-B.
           DIVIDE WS-A BY WS-B GIVING WS-RESULT.
           IF WS-RESULT = 4567
               DISPLAY "NC105A-TEST-3 PASS"
           ELSE
               DISPLAY "NC105A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
