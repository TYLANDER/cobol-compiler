       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC202A.
      *
      * NIST CCVS-style test: ADD/SUBTRACT with multiple operands
      * Tests ADD A B C TO D, ADD A B C GIVING D,
      * and SUBTRACT A B C FROM D.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(4) VALUE ZEROS.
       01 WS-B            PIC 9(4) VALUE ZEROS.
       01 WS-C            PIC 9(4) VALUE ZEROS.
       01 WS-D            PIC 9(4) VALUE ZEROS.
       01 WS-E            PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: ADD A B C TO D
      *   A=10, B=20, C=30, D=40 => D = 40 + 10 + 20 + 30 = 100
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 30 TO WS-C.
           MOVE 40 TO WS-D.
           ADD WS-A WS-B WS-C TO WS-D.
           IF WS-D = 100
               DISPLAY "NC202A-TEST-1 PASS"
           ELSE
               DISPLAY "NC202A-TEST-1 FAIL"
               DISPLAY "  Expected D=100, got " WS-D
           END-IF.
      * Test 2: ADD A B C GIVING E
      *   A=5, B=15, C=25 => E = 5 + 15 + 25 = 45
           MOVE 5 TO WS-A.
           MOVE 15 TO WS-B.
           MOVE 25 TO WS-C.
           MOVE 0 TO WS-E.
           ADD WS-A WS-B WS-C GIVING WS-E.
           IF WS-E = 45
               DISPLAY "NC202A-TEST-2 PASS"
           ELSE
               DISPLAY "NC202A-TEST-2 FAIL"
               DISPLAY "  Expected E=45, got " WS-E
           END-IF.
      * Test 3: SUBTRACT A B FROM D
      *   A=10, B=20, D=100 => D = 100 - 10 - 20 = 70
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 100 TO WS-D.
           SUBTRACT WS-A WS-B FROM WS-D.
           IF WS-D = 70
               DISPLAY "NC202A-TEST-3 PASS"
           ELSE
               DISPLAY "NC202A-TEST-3 FAIL"
               DISPLAY "  Expected D=70, got " WS-D
           END-IF.
           STOP RUN.
