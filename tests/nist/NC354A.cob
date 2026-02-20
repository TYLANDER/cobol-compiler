       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC354A.
      *
      * NIST CCVS-style test: Arithmetic with COMP (BINARY)
      * fields. Tests ADD, SUBTRACT, and MULTIPLY with
      * PIC S9(4) COMP fields.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COMP-A         PIC S9(4) COMP VALUE 100.
       01 WS-COMP-B         PIC S9(4) COMP VALUE 250.
       01 WS-COMP-C         PIC S9(4) COMP VALUE 0.
       01 WS-COMP-D         PIC S9(4) COMP VALUE 0.
       01 WS-COMP-E         PIC S9(4) COMP VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: ADD with COMP fields
      *   ADD 100 + 250, result should be 350.
           ADD WS-COMP-A TO WS-COMP-B
               GIVING WS-COMP-C.
           IF WS-COMP-C = 350
               DISPLAY "NC354A-TEST-1 PASS"
           ELSE
               DISPLAY "NC354A-TEST-1 FAIL"
               DISPLAY "  Expected 350, got " WS-COMP-C
           END-IF.
      * Test 2: SUBTRACT with COMP fields
      *   SUBTRACT 100 FROM 350, result should be 250.
           SUBTRACT WS-COMP-A FROM WS-COMP-C
               GIVING WS-COMP-D.
           IF WS-COMP-D = 250
               DISPLAY "NC354A-TEST-2 PASS"
           ELSE
               DISPLAY "NC354A-TEST-2 FAIL"
               DISPLAY "  Expected 250, got " WS-COMP-D
           END-IF.
      * Test 3: MULTIPLY with COMP fields
      *   MULTIPLY 100 BY 25, result should be 2500.
           MOVE 25 TO WS-COMP-B.
           MULTIPLY WS-COMP-A BY WS-COMP-B
               GIVING WS-COMP-E.
           IF WS-COMP-E = 2500
               DISPLAY "NC354A-TEST-3 PASS"
           ELSE
               DISPLAY "NC354A-TEST-3 FAIL"
               DISPLAY "  Expected 2500, got " WS-COMP-E
           END-IF.
           STOP RUN.
