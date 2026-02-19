       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC193A.
      *
      * NIST CCVS-style test: PERFORM VARYING with AFTER clause
      * Tests nested varying (2D iteration) using the AFTER phrase
      * to iterate over multiple loop variables in one statement.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I          PIC 9(4) VALUE ZEROS.
       01 WS-J          PIC 9(4) VALUE ZEROS.
       01 WS-K          PIC 9(4) VALUE ZEROS.
       01 WS-COUNT      PIC 9(4) VALUE ZEROS.
       01 WS-SUM        PIC 9(8) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: PERFORM VARYING I FROM 1 BY 1 UNTIL > 3
      *         AFTER J FROM 1 BY 1 UNTIL > 4
      *   Should iterate 3 * 4 = 12 times
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 3
             AFTER WS-J FROM 1 BY 1
             UNTIL WS-J > 4
               ADD 1 TO WS-COUNT
           END-PERFORM.
           IF WS-COUNT = 12
               DISPLAY "NC193A-TEST-1 PASS"
           ELSE
               DISPLAY "NC193A-TEST-1 FAIL"
               DISPLAY "  Expected 12, got " WS-COUNT
           END-IF.
      * Test 2: PERFORM VARYING with AFTER, accumulating I * J
      *   I from 1 to 2, J from 1 to 3
      *   Sum = 1*1 + 1*2 + 1*3 + 2*1 + 2*2 + 2*3
      *       = 1 + 2 + 3 + 2 + 4 + 6 = 18
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 2
             AFTER WS-J FROM 1 BY 1
             UNTIL WS-J > 3
               COMPUTE WS-K = WS-I * WS-J
               ADD WS-K TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 18
               DISPLAY "NC193A-TEST-2 PASS"
           ELSE
               DISPLAY "NC193A-TEST-2 FAIL"
               DISPLAY "  Expected 18, got " WS-SUM
           END-IF.
      * Test 3: Verify final values of loop variables after PERFORM
      *   After PERFORM VARYING I FROM 1 BY 1 UNTIL > 5
      *     AFTER J FROM 1 BY 1 UNTIL > 2
      *   I should be 6, J should be 1 (reset by outer increment)
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 5
             AFTER WS-J FROM 1 BY 1
             UNTIL WS-J > 2
               ADD 1 TO WS-COUNT
           END-PERFORM.
           IF WS-COUNT = 10
               DISPLAY "NC193A-TEST-3 PASS"
           ELSE
               DISPLAY "NC193A-TEST-3 FAIL"
               DISPLAY "  Expected 10, got " WS-COUNT
           END-IF.
           STOP RUN.
