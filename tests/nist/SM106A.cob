       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM106A.
      *
      * NIST CCVS-style test: REPLACE statement
      * Tests the REPLACE directive for text substitution.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VALUE PIC X(10) VALUE SPACES.
       REPLACE ==PLACEHOLDER== BY ==WS-VALUE==.
       PROCEDURE DIVISION.
      * Test 1: REPLACE substitution in MOVE
           MOVE "REPLACED  " TO PLACEHOLDER.
           IF WS-VALUE = "REPLACED  "
               DISPLAY "SM106A-TEST-1 PASS"
           ELSE
               DISPLAY "SM106A-TEST-1 FAIL"
               DISPLAY "  VAL=>" WS-VALUE "<"
           END-IF.
       REPLACE OFF.
      * Test 2: After REPLACE OFF, original text used
      *   (PLACEHOLDER is no longer replaced; this should cause
      *    a compilation error if used. Instead, test that WS-VALUE
      *    retained its value from Test 1.)
           IF WS-VALUE = "REPLACED  "
               DISPLAY "SM106A-TEST-2 PASS"
           ELSE
               DISPLAY "SM106A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
