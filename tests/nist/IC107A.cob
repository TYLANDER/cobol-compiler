       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC107A.
      *
      * NIST CCVS-style test: Subprogram with multiple operations
      * Tests CALL passing three params: two inputs and one output.
      * The subprogram multiplies the two inputs and stores in output.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-X PIC 9(4) VALUE 0.
       01 WS-Y PIC 9(4) VALUE 0.
       01 WS-RESULT PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: 5 * 6 = 30
           MOVE 5 TO WS-X.
           MOVE 6 TO WS-Y.
           CALL "IC107A-SUB" USING WS-X WS-Y WS-RESULT.
           IF WS-RESULT = 30
               DISPLAY "IC107A-TEST-1 PASS"
           ELSE
               DISPLAY "IC107A-TEST-1 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
           END-IF.
      * Test 2: 12 * 3 = 36
           MOVE 12 TO WS-X.
           MOVE 3 TO WS-Y.
           CALL "IC107A-SUB" USING WS-X WS-Y WS-RESULT.
           IF WS-RESULT = 36
               DISPLAY "IC107A-TEST-2 PASS"
           ELSE
               DISPLAY "IC107A-TEST-2 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
           END-IF.
           STOP RUN.
