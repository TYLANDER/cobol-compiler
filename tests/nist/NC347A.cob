       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC347A.
      *
      * NIST CCVS-style test: PERFORM with inline statements
      * Tests PERFORM ... END-PERFORM with multiple statements
      * in the inline body, including VARYING and UNTIL forms.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER      PIC 9(4) VALUE 0.
       01 WS-SUM           PIC 9(6) VALUE 0.
       01 WS-I             PIC 9(4) VALUE 0.
       01 WS-TEMP          PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Inline PERFORM UNTIL with multiple statements
      *   Sum integers 1 through 5 = 15
           MOVE 0 TO WS-COUNTER.
           MOVE 0 TO WS-SUM.
           PERFORM UNTIL WS-COUNTER = 5
               ADD 1 TO WS-COUNTER
               ADD WS-COUNTER TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 15
               DISPLAY "NC347A-TEST-1 PASS"
           ELSE
               DISPLAY "NC347A-TEST-1 FAIL"
               DISPLAY "  Expected 15, got " WS-SUM
           END-IF.
      * Test 2: Inline PERFORM VARYING
      *   Sum 10 + 20 + 30 + 40 + 50 = 150
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 10 BY 10
               UNTIL WS-I > 50
               ADD WS-I TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 150
               DISPLAY "NC347A-TEST-2 PASS"
           ELSE
               DISPLAY "NC347A-TEST-2 FAIL"
               DISPLAY "  Expected 150, got " WS-SUM
           END-IF.
      * Test 3: Inline PERFORM with IF inside body
      *   Count how many of 1..10 are greater than 5
           MOVE 0 TO WS-COUNTER.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 10
               IF WS-I > 5
                   ADD 1 TO WS-COUNTER
               END-IF
           END-PERFORM.
           IF WS-COUNTER = 5
               DISPLAY "NC347A-TEST-3 PASS"
           ELSE
               DISPLAY "NC347A-TEST-3 FAIL"
               DISPLAY "  Expected 5, got " WS-COUNTER
           END-IF.
           STOP RUN.
