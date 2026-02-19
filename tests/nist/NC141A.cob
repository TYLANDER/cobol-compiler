       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC141A.
      *
      * NIST CCVS-style test: PERFORM VARYING with AFTER
      * Tests nested PERFORM VARYING loops using the AFTER
      * clause to iterate over two indices simultaneously.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I            PIC 9(4) VALUE ZEROS.
       01 WS-J            PIC 9(4) VALUE ZEROS.
       01 WS-COUNT        PIC 9(4) VALUE ZEROS.
       01 WS-SUM          PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
      *         AFTER J FROM 1 BY 1 UNTIL J > 2
      *   Inner loop runs 2 times for each of 3 outer iterations
      *   Total iterations = 3 * 2 = 6
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               AFTER WS-J FROM 1 BY 1
               UNTIL WS-J > 2
               ADD 1 TO WS-COUNT
           END-PERFORM.
           IF WS-COUNT = 6
               DISPLAY "NC141A-TEST-1 PASS"
           ELSE
               DISPLAY "NC141A-TEST-1 FAIL"
               DISPLAY "  Expected COUNT=6, got " WS-COUNT
           END-IF.
      * Test 2: Accumulate I + J in each nested iteration
      *   I=1,J=1 => 2; I=1,J=2 => 3;
      *   I=2,J=1 => 3; I=2,J=2 => 4;
      *   I=3,J=1 => 4; I=3,J=2 => 5;
      *   Sum = 2+3+3+4+4+5 = 21
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3
               AFTER WS-J FROM 1 BY 1
               UNTIL WS-J > 2
               ADD WS-I TO WS-SUM
               ADD WS-J TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 21
               DISPLAY "NC141A-TEST-2 PASS"
           ELSE
               DISPLAY "NC141A-TEST-2 FAIL"
               DISPLAY "  Expected SUM=21, got " WS-SUM
           END-IF.
           STOP RUN.
