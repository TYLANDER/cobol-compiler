       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC212A.
      *
      * NIST CCVS-style test: Nested PERFORM VARYING
      * Tests inner loop accumulation with PERFORM VARYING
      * inside PERFORM VARYING.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I            PIC 9(4) VALUE ZEROS.
       01 WS-J            PIC 9(4) VALUE ZEROS.
       01 WS-SUM          PIC 9(4) VALUE ZEROS.
       01 WS-COUNT        PIC 9(4) VALUE ZEROS.
       01 WS-PRODUCT      PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Nested PERFORM VARYING - count iterations
      *   Outer: i = 1 to 3, Inner: j = 1 to 4
      *   Total iterations = 3 * 4 = 12
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM 1 BY 1
                 UNTIL WS-J > 4
                   ADD 1 TO WS-COUNT
               END-PERFORM
           END-PERFORM.
           IF WS-COUNT = 12
               DISPLAY "NC212A-TEST-1 PASS"
           ELSE
               DISPLAY "NC212A-TEST-1 FAIL"
               DISPLAY "  Expected COUNT=12, got COUNT="
                   WS-COUNT
           END-IF.
      * Test 2: Nested PERFORM VARYING - accumulate sum
      *   Outer: i = 1 to 3, Inner: j = 1 to 3
      *   Sum of (i + j) for all pairs:
      *   (1+1)+(1+2)+(1+3)+(2+1)+(2+2)+(2+3)+(3+1)+(3+2)+(3+3)
      *   = 2+3+4+3+4+5+4+5+6 = 36
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM 1 BY 1
                 UNTIL WS-J > 3
                   ADD WS-I TO WS-SUM
                   ADD WS-J TO WS-SUM
               END-PERFORM
           END-PERFORM.
           IF WS-SUM = 36
               DISPLAY "NC212A-TEST-2 PASS"
           ELSE
               DISPLAY "NC212A-TEST-2 FAIL"
               DISPLAY "  Expected SUM=36, got SUM="
                   WS-SUM
           END-IF.
      * Test 3: Nested PERFORM VARYING with BY 2
      *   Outer: i = 1,3,5 (BY 2 UNTIL > 5)
      *   Inner: j = 1,3 (BY 2 UNTIL > 3)
      *   Count = 3 * 2 = 6
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 2
             UNTIL WS-I > 5
               PERFORM VARYING WS-J FROM 1 BY 2
                 UNTIL WS-J > 3
                   ADD 1 TO WS-COUNT
               END-PERFORM
           END-PERFORM.
           IF WS-COUNT = 6
               DISPLAY "NC212A-TEST-3 PASS"
           ELSE
               DISPLAY "NC212A-TEST-3 FAIL"
               DISPLAY "  Expected COUNT=6, got COUNT="
                   WS-COUNT
           END-IF.
           STOP RUN.
