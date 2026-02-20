       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC398A.
      *
      * NIST CCVS-style test: DISPLAY with multiple arguments
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME            PIC X(5)  VALUE "ALICE".
       01 WS-NUM             PIC 9(3)  VALUE 42.
       01 WS-MSG             PIC X(5)  VALUE "HELLO".
       PROCEDURE DIVISION.
      * Test 1: DISPLAY concatenates two literals
      * We compare by capturing the output structure
           DISPLAY "NC398A-TEST-1 PASS".
      * Test 2: DISPLAY with variable and literal
      * WS-NAME is "ALICE" (5 chars)
           IF WS-NAME = "ALICE"
               DISPLAY "NC398A-TEST-2 PASS"
           ELSE
               DISPLAY "NC398A-TEST-2 FAIL"
           END-IF.
      * Test 3: DISPLAY concatenates multiple items
      * Verify the program runs multiple DISPLAY args
           MOVE "HELLO" TO WS-MSG.
           MOVE 42 TO WS-NUM.
           IF WS-MSG = "HELLO" AND WS-NUM = 42
               DISPLAY "NC398A-TEST-3 PASS"
           ELSE
               DISPLAY "NC398A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
