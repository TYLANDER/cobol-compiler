       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC101A.
      *
      * NIST CCVS-style test: MOVE and DISPLAY
      * Tests basic MOVE statement and DISPLAY output.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TEST-1 PIC X(5)  VALUE SPACES.
       01 WS-TEST-2 PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: MOVE alphanumeric literal to PIC X field
           MOVE "ABCDE" TO WS-TEST-1.
           IF WS-TEST-1 = "ABCDE"
               DISPLAY "NC101A-TEST-1 PASS"
           ELSE
               DISPLAY "NC101A-TEST-1 FAIL"
           END-IF.
      * Test 2: MOVE numeric literal to PIC 9 field
           MOVE 1234 TO WS-TEST-2.
           IF WS-TEST-2 = 1234
               DISPLAY "NC101A-TEST-2 PASS"
           ELSE
               DISPLAY "NC101A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
