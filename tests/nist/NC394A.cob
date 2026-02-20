       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC394A.
      *
      * NIST CCVS-style test: ADD/SUBTRACT CORRESPONDING
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP-A.
           05 WS-X            PIC 9(3) VALUE 0.
           05 WS-Y            PIC 9(3) VALUE 0.
           05 WS-Z            PIC 9(3) VALUE 0.
       01 WS-GROUP-B.
           05 WS-X            PIC 9(3) VALUE 0.
           05 WS-Y            PIC 9(3) VALUE 0.
           05 WS-Z            PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: ADD CORRESPONDING
           MOVE 10 TO WS-X OF WS-GROUP-A.
           MOVE 20 TO WS-Y OF WS-GROUP-A.
           MOVE 30 TO WS-Z OF WS-GROUP-A.
           MOVE 1 TO WS-X OF WS-GROUP-B.
           MOVE 2 TO WS-Y OF WS-GROUP-B.
           MOVE 3 TO WS-Z OF WS-GROUP-B.
           ADD CORRESPONDING WS-GROUP-B TO WS-GROUP-A.
           IF WS-X OF WS-GROUP-A = 11
               AND WS-Y OF WS-GROUP-A = 22
               AND WS-Z OF WS-GROUP-A = 33
               DISPLAY "NC394A-TEST-1 PASS"
           ELSE
               DISPLAY "NC394A-TEST-1 FAIL"
               DISPLAY "  X=" WS-X OF WS-GROUP-A
               DISPLAY "  Y=" WS-Y OF WS-GROUP-A
               DISPLAY "  Z=" WS-Z OF WS-GROUP-A
           END-IF.
      * Test 2: SUBTRACT CORRESPONDING
           MOVE 100 TO WS-X OF WS-GROUP-A.
           MOVE 200 TO WS-Y OF WS-GROUP-A.
           MOVE 300 TO WS-Z OF WS-GROUP-A.
           MOVE 10 TO WS-X OF WS-GROUP-B.
           MOVE 20 TO WS-Y OF WS-GROUP-B.
           MOVE 30 TO WS-Z OF WS-GROUP-B.
           SUBTRACT CORRESPONDING WS-GROUP-B
               FROM WS-GROUP-A.
           IF WS-X OF WS-GROUP-A = 90
               AND WS-Y OF WS-GROUP-A = 180
               AND WS-Z OF WS-GROUP-A = 270
               DISPLAY "NC394A-TEST-2 PASS"
           ELSE
               DISPLAY "NC394A-TEST-2 FAIL"
               DISPLAY "  X=" WS-X OF WS-GROUP-A
               DISPLAY "  Y=" WS-Y OF WS-GROUP-A
               DISPLAY "  Z=" WS-Z OF WS-GROUP-A
           END-IF.
      * Test 3: ADD CORR with different initial values
           MOVE 0 TO WS-X OF WS-GROUP-A.
           MOVE 0 TO WS-Y OF WS-GROUP-A.
           MOVE 0 TO WS-Z OF WS-GROUP-A.
           MOVE 50 TO WS-X OF WS-GROUP-B.
           MOVE 60 TO WS-Y OF WS-GROUP-B.
           MOVE 70 TO WS-Z OF WS-GROUP-B.
           ADD CORR WS-GROUP-B TO WS-GROUP-A.
           IF WS-X OF WS-GROUP-A = 50
               AND WS-Y OF WS-GROUP-A = 60
               AND WS-Z OF WS-GROUP-A = 70
               DISPLAY "NC394A-TEST-3 PASS"
           ELSE
               DISPLAY "NC394A-TEST-3 FAIL"
               DISPLAY "  X=" WS-X OF WS-GROUP-A
               DISPLAY "  Y=" WS-Y OF WS-GROUP-A
               DISPLAY "  Z=" WS-Z OF WS-GROUP-A
           END-IF.
           STOP RUN.
