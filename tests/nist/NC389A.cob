       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC389A.
      *
      * NIST CCVS-style test: Nested PERFORM
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OUTER           PIC 9(3) VALUE 0.
       01 WS-INNER           PIC 9(3) VALUE 0.
       01 WS-TOTAL           PIC 9(3) VALUE 0.
       01 WS-I               PIC 9(3) VALUE 0.
       01 WS-J               PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Nested inline PERFORM
           MOVE 0 TO WS-TOTAL.
           PERFORM 3 TIMES
               PERFORM 4 TIMES
                   ADD 1 TO WS-TOTAL
               END-PERFORM
           END-PERFORM.
           IF WS-TOTAL = 12
               DISPLAY "NC389A-TEST-1 PASS"
           ELSE
               DISPLAY "NC389A-TEST-1 FAIL"
               DISPLAY "  Expected 12, got " WS-TOTAL
           END-IF.
      * Test 2: Perform para that performs another para
           MOVE 0 TO WS-TOTAL.
           PERFORM NC389A-OUTER-PARA 2 TIMES.
           IF WS-TOTAL = 6
               DISPLAY "NC389A-TEST-2 PASS"
           ELSE
               DISPLAY "NC389A-TEST-2 FAIL"
               DISPLAY "  Expected 6, got " WS-TOTAL
           END-IF.
      * Test 3: Nested PERFORM VARYING
           MOVE 0 TO WS-TOTAL.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-I
                   ADD 1 TO WS-TOTAL
               END-PERFORM
           END-PERFORM.
           IF WS-TOTAL = 6
               DISPLAY "NC389A-TEST-3 PASS"
           ELSE
               DISPLAY "NC389A-TEST-3 FAIL"
               DISPLAY "  Expected 6, got " WS-TOTAL
           END-IF.
           STOP RUN.
       NC389A-OUTER-PARA.
           PERFORM NC389A-INNER-PARA 3 TIMES.
       NC389A-INNER-PARA.
           ADD 1 TO WS-TOTAL.
