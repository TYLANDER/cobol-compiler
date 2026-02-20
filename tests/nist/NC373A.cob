       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC373A.
      *
      * NIST CCVS-style test: Complex group MOVE (group to
      * group with different layouts).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP-A.
           05 WS-A-PART1  PIC X(5)  VALUE "ABCDE".
           05 WS-A-PART2  PIC X(5)  VALUE "FGHIJ".
       01 WS-GROUP-B.
           05 WS-B-PART1  PIC X(3)  VALUE SPACES.
           05 WS-B-PART2  PIC X(7)  VALUE SPACES.
       01 WS-GROUP-C.
           05 WS-C-PART1  PIC X(10) VALUE SPACES.
       01 WS-GROUP-D.
           05 WS-D-PART1  PIC X(4)  VALUE SPACES.
           05 WS-D-PART2  PIC X(4)  VALUE SPACES.
           05 WS-D-PART3  PIC X(2)  VALUE SPACES.
       01 WS-GROUP-E.
           05 WS-E-PART1  PIC X(8)  VALUE "12345678".
           05 WS-E-PART2  PIC X(2)  VALUE "90".
       PROCEDURE DIVISION.
      * Test 1: MOVE group A (5+5=10) to group B (3+7=10)
      * B-PART1 gets "ABC", B-PART2 gets "DEFGHIJ"
           MOVE WS-GROUP-A TO WS-GROUP-B.
           IF WS-B-PART1 = "ABC" AND WS-B-PART2 = "DEFGHIJ"
               DISPLAY "NC373A-TEST-1 PASS"
           ELSE
               DISPLAY "NC373A-TEST-1 FAIL"
               DISPLAY "  B-PART1=" WS-B-PART1
               DISPLAY "  B-PART2=" WS-B-PART2
           END-IF.
      * Test 2: MOVE group A (10) to group C (10)
           MOVE WS-GROUP-A TO WS-GROUP-C.
           IF WS-C-PART1 = "ABCDEFGHIJ"
               DISPLAY "NC373A-TEST-2 PASS"
           ELSE
               DISPLAY "NC373A-TEST-2 FAIL"
               DISPLAY "  C-PART1=" WS-C-PART1
           END-IF.
      * Test 3: MOVE group E (8+2=10) to group D (4+4+2)
           MOVE WS-GROUP-E TO WS-GROUP-D.
           IF WS-D-PART1 = "1234" AND WS-D-PART2 = "5678"
               AND WS-D-PART3 = "90"
               DISPLAY "NC373A-TEST-3 PASS"
           ELSE
               DISPLAY "NC373A-TEST-3 FAIL"
               DISPLAY "  D-PART1=" WS-D-PART1
               DISPLAY "  D-PART2=" WS-D-PART2
               DISPLAY "  D-PART3=" WS-D-PART3
           END-IF.
           STOP RUN.
