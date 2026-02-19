       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC263A.
      *
      * NIST CCVS-style test: SUBTRACT CORRESPONDING
      * Tests SUBTRACT CORR between groups.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC.
           05 WS-A     PIC 9(3) VALUE 50.
           05 WS-B     PIC 9(3) VALUE 100.
           05 WS-C     PIC 9(3) VALUE 25.
       01 WS-DST.
           05 WS-A     PIC 9(3) VALUE 200.
           05 WS-B     PIC 9(3) VALUE 300.
           05 WS-D     PIC 9(3) VALUE 999.
       PROCEDURE DIVISION.
      * Test 1: SUBTRACT CORR - matching fields subtracted
      *   SRC: A=50 B=100; DST: A=200 B=300
      *   After: A=150 B=200
           MOVE 200 TO WS-A OF WS-DST.
           MOVE 300 TO WS-B OF WS-DST.
           MOVE 999 TO WS-D OF WS-DST.
           SUBTRACT CORRESPONDING WS-SRC FROM WS-DST.
           IF WS-A OF WS-DST = 150
               AND WS-B OF WS-DST = 200
               DISPLAY "NC263A-TEST-1 PASS"
           ELSE
               DISPLAY "NC263A-TEST-1 FAIL"
               DISPLAY "  A=" WS-A OF WS-DST
               DISPLAY "  B=" WS-B OF WS-DST
           END-IF.
      * Test 2: Non-matching field unchanged
           IF WS-D OF WS-DST = 999
               DISPLAY "NC263A-TEST-2 PASS"
           ELSE
               DISPLAY "NC263A-TEST-2 FAIL"
               DISPLAY "  D should be 999, got "
                   WS-D OF WS-DST
           END-IF.
      * Test 3: Source fields unchanged
           IF WS-A OF WS-SRC = 50
               AND WS-B OF WS-SRC = 100
               DISPLAY "NC263A-TEST-3 PASS"
           ELSE
               DISPLAY "NC263A-TEST-3 FAIL"
               DISPLAY "  SRC A=" WS-A OF WS-SRC
                   " B=" WS-B OF WS-SRC
           END-IF.
           STOP RUN.
