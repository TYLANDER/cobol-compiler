       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC203A.
      *
      * NIST CCVS-style test: CALL with multiple parameter types
      * Tests passing numeric and alphanumeric parameters together
      * in a single CALL statement.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME   PIC X(10) VALUE SPACES.
       01 WS-AGE    PIC 9(3) VALUE 0.
       01 WS-SALARY PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: All three parameters set by subprogram
           CALL "IC203A-SUB" USING WS-NAME WS-AGE WS-SALARY.
           IF WS-NAME = "JOHN      "
               DISPLAY "IC203A-TEST-1 PASS"
           ELSE
               DISPLAY "IC203A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-NAME "<"
           END-IF.
      * Test 2: Numeric age parameter set correctly
           IF WS-AGE = 30
               DISPLAY "IC203A-TEST-2 PASS"
           ELSE
               DISPLAY "IC203A-TEST-2 FAIL"
               DISPLAY "  AGE=" WS-AGE
           END-IF.
      * Test 3: Larger numeric salary parameter set correctly
           IF WS-SALARY = 50000
               DISPLAY "IC203A-TEST-3 PASS"
           ELSE
               DISPLAY "IC203A-TEST-3 FAIL"
               DISPLAY "  SALARY=" WS-SALARY
           END-IF.
           STOP RUN.
