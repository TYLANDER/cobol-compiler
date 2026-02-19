       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC101A.
      *
      * NIST CCVS-style test: Basic CALL statement
      * Tests CALL with USING, passing BY REFERENCE.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Basic CALL with one parameter
           CALL "IC101A-SUB" USING WS-RESULT.
           IF WS-RESULT = 42
               DISPLAY "IC101A-TEST-1 PASS"
           ELSE
               DISPLAY "IC101A-TEST-1 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
           END-IF.
      * Test 2: CALL again - verify can be called multiple times
           MOVE 0 TO WS-RESULT.
           CALL "IC101A-SUB" USING WS-RESULT.
           IF WS-RESULT = 42
               DISPLAY "IC101A-TEST-2 PASS"
           ELSE
               DISPLAY "IC101A-TEST-2 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
           END-IF.
           STOP RUN.
