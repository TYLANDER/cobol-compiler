       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC245A.
      *
      * NIST CCVS-style test: IF with compound conditions
      * Tests AND, OR, and NOT in compound IF conditions.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC 9(4) VALUE ZEROS.
       01 WS-B        PIC 9(4) VALUE ZEROS.
       01 WS-C        PIC 9(4) VALUE ZEROS.
       01 WS-FLAG     PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: IF with AND - both conditions must be true
      *   A=10 B=20 => A < B AND B > 15 should be true
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 0 TO WS-FLAG.
           IF WS-A < WS-B AND WS-B > 15
               MOVE 1 TO WS-FLAG
           END-IF.
           IF WS-FLAG = 1
               DISPLAY "NC245A-TEST-1 PASS"
           ELSE
               DISPLAY "NC245A-TEST-1 FAIL"
               DISPLAY "  AND condition should be true"
           END-IF.
      * Test 2: IF with OR - at least one condition true
      *   A=10 B=5 => A > 100 OR B < 10 should be true
           MOVE 10 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE 0 TO WS-FLAG.
           IF WS-A > 100 OR WS-B < 10
               MOVE 1 TO WS-FLAG
           END-IF.
           IF WS-FLAG = 1
               DISPLAY "NC245A-TEST-2 PASS"
           ELSE
               DISPLAY "NC245A-TEST-2 FAIL"
               DISPLAY "  OR condition should be true"
           END-IF.
      * Test 3: IF with NOT and mixed AND/OR
      *   A=10 B=20 C=30
      *   NOT A = 99 AND (B = 20 OR C = 99) should be true
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 30 TO WS-C.
           MOVE 0 TO WS-FLAG.
           IF NOT WS-A = 99 AND
              (WS-B = 20 OR WS-C = 99)
               MOVE 1 TO WS-FLAG
           END-IF.
           IF WS-FLAG = 1
               DISPLAY "NC245A-TEST-3 PASS"
           ELSE
               DISPLAY "NC245A-TEST-3 FAIL"
               DISPLAY "  NOT/AND/OR condition failed"
           END-IF.
           STOP RUN.
