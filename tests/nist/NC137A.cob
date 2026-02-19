       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC137A.
      *
      * NIST CCVS-style test: PERFORM VARYING
      * Tests PERFORM VARYING with different step values.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER     PIC 9(4) VALUE ZEROS.
       01 WS-SUM          PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: PERFORM VARYING counter FROM 1 BY 1
      *   UNTIL counter > 5
      *   Sum of 1+2+3+4+5 = 15
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 5
               ADD WS-COUNTER TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 15
               DISPLAY "NC137A-TEST-1 PASS"
           ELSE
               DISPLAY "NC137A-TEST-1 FAIL"
               DISPLAY "  Expected SUM=15, got " WS-SUM
           END-IF.
      * Test 2: PERFORM VARYING with BY 2
      *   counter takes values 1, 3, 5; sum = 9
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-COUNTER FROM 1 BY 2
               UNTIL WS-COUNTER > 5
               ADD WS-COUNTER TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 9
               DISPLAY "NC137A-TEST-2 PASS"
           ELSE
               DISPLAY "NC137A-TEST-2 FAIL"
               DISPLAY "  Expected SUM=9, got " WS-SUM
           END-IF.
           STOP RUN.
