       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC241A.
      *
      * NIST CCVS-style test: ADD/SUBTRACT with multiple operands
      * Tests ADD A B C TO D and SUBTRACT A B FROM C.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC 9(4) VALUE 10.
       01 WS-B        PIC 9(4) VALUE 20.
       01 WS-C        PIC 9(4) VALUE 30.
       01 WS-D        PIC 9(4) VALUE 100.
       01 WS-E        PIC 9(4) VALUE 500.
       01 WS-F        PIC 9(4) VALUE 1000.
       PROCEDURE DIVISION.
      * Test 1: ADD A B C TO D
      *   D should become 100 + 10 + 20 + 30 = 160
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 30 TO WS-C.
           MOVE 100 TO WS-D.
           ADD WS-A WS-B WS-C TO WS-D.
           IF WS-D = 160
               DISPLAY "NC241A-TEST-1 PASS"
           ELSE
               DISPLAY "NC241A-TEST-1 FAIL"
               DISPLAY "  Expected 160, got " WS-D
           END-IF.
      * Test 2: SUBTRACT A B FROM C
      *   E should become 500 - 10 - 20 = 470
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 500 TO WS-E.
           SUBTRACT WS-A WS-B FROM WS-E.
           IF WS-E = 470
               DISPLAY "NC241A-TEST-2 PASS"
           ELSE
               DISPLAY "NC241A-TEST-2 FAIL"
               DISPLAY "  Expected 470, got " WS-E
           END-IF.
      * Test 3: ADD A B GIVING C
      *   F should be 10 + 20 = 30, WS-A and WS-B unchanged
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           ADD WS-A WS-B GIVING WS-F.
           IF WS-F = 30 AND WS-A = 10 AND WS-B = 20
               DISPLAY "NC241A-TEST-3 PASS"
           ELSE
               DISPLAY "NC241A-TEST-3 FAIL"
               DISPLAY "  Expected F=30 A=10 B=20"
               DISPLAY "  Got F=" WS-F " A=" WS-A
                   " B=" WS-B
           END-IF.
           STOP RUN.
