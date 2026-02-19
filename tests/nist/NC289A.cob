       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC289A.
      *
      * NIST CCVS-style test: ADD/SUBTRACT CORRESPONDING
      * Tests ADD CORRESPONDING and SUBTRACT CORRESPONDING
      * between group items with matching field names.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP-A.
           05 WS-X          PIC 9(4) VALUE ZEROS.
           05 WS-Y          PIC 9(4) VALUE ZEROS.
           05 WS-Z          PIC 9(4) VALUE ZEROS.
       01 WS-GROUP-B.
           05 WS-X          PIC 9(4) VALUE ZEROS.
           05 WS-Y          PIC 9(4) VALUE ZEROS.
           05 WS-Z          PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: ADD CORRESPONDING
      *   Group-A: X=10, Y=20, Z=30
      *   Group-B: X=5,  Y=15, Z=25
      *   After ADD CORR A TO B => B: X=15, Y=35, Z=55
           MOVE 10 TO WS-X OF WS-GROUP-A.
           MOVE 20 TO WS-Y OF WS-GROUP-A.
           MOVE 30 TO WS-Z OF WS-GROUP-A.
           MOVE 5 TO WS-X OF WS-GROUP-B.
           MOVE 15 TO WS-Y OF WS-GROUP-B.
           MOVE 25 TO WS-Z OF WS-GROUP-B.
           ADD CORRESPONDING WS-GROUP-A TO WS-GROUP-B.
           IF WS-X OF WS-GROUP-B = 15
               AND WS-Y OF WS-GROUP-B = 35
               AND WS-Z OF WS-GROUP-B = 55
               DISPLAY "NC289A-TEST-1 PASS"
           ELSE
               DISPLAY "NC289A-TEST-1 FAIL"
               DISPLAY "  Expected X=15 Y=35 Z=55"
               DISPLAY "  Got X=" WS-X OF WS-GROUP-B
                   " Y=" WS-Y OF WS-GROUP-B
                   " Z=" WS-Z OF WS-GROUP-B
           END-IF.
      * Test 2: SUBTRACT CORRESPONDING
      *   Group-A: X=3, Y=5, Z=10
      *   Group-B: X=15, Y=35, Z=55 (from Test 1)
      *   After SUBTRACT CORR A FROM B => B: X=12, Y=30, Z=45
           MOVE 3 TO WS-X OF WS-GROUP-A.
           MOVE 5 TO WS-Y OF WS-GROUP-A.
           MOVE 10 TO WS-Z OF WS-GROUP-A.
           SUBTRACT CORRESPONDING WS-GROUP-A
               FROM WS-GROUP-B.
           IF WS-X OF WS-GROUP-B = 12
               AND WS-Y OF WS-GROUP-B = 30
               AND WS-Z OF WS-GROUP-B = 45
               DISPLAY "NC289A-TEST-2 PASS"
           ELSE
               DISPLAY "NC289A-TEST-2 FAIL"
               DISPLAY "  Expected X=12 Y=30 Z=45"
               DISPLAY "  Got X=" WS-X OF WS-GROUP-B
                   " Y=" WS-Y OF WS-GROUP-B
                   " Z=" WS-Z OF WS-GROUP-B
           END-IF.
      * Test 3: Source group unchanged after operations
      *   Group-A should still have X=3, Y=5, Z=10
           IF WS-X OF WS-GROUP-A = 3
               AND WS-Y OF WS-GROUP-A = 5
               AND WS-Z OF WS-GROUP-A = 10
               DISPLAY "NC289A-TEST-3 PASS"
           ELSE
               DISPLAY "NC289A-TEST-3 FAIL"
               DISPLAY "  Source group modified"
               DISPLAY "  Got X=" WS-X OF WS-GROUP-A
                   " Y=" WS-Y OF WS-GROUP-A
                   " Z=" WS-Z OF WS-GROUP-A
           END-IF.
           STOP RUN.
