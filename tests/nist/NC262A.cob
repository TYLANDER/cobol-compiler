       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC262A.
      *
      * NIST CCVS-style test: ADD CORRESPONDING between groups
      * Tests ADD CORR with matching and non-matching fields.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC.
           05 WS-A     PIC 9(3) VALUE 100.
           05 WS-B     PIC 9(3) VALUE 200.
           05 WS-C     PIC 9(3) VALUE 50.
       01 WS-DST.
           05 WS-A     PIC 9(3) VALUE 150.
           05 WS-B     PIC 9(3) VALUE 300.
           05 WS-D     PIC 9(3) VALUE 999.
       PROCEDURE DIVISION.
      * Test 1: ADD CORR - matching fields get added
      *   SRC: A=100 B=200; DST: A=150 B=300
      *   After: A=250 B=500
           MOVE 150 TO WS-A OF WS-DST.
           MOVE 300 TO WS-B OF WS-DST.
           MOVE 999 TO WS-D OF WS-DST.
           ADD CORRESPONDING WS-SRC TO WS-DST.
           IF WS-A OF WS-DST = 250
               AND WS-B OF WS-DST = 500
               DISPLAY "NC262A-TEST-1 PASS"
           ELSE
               DISPLAY "NC262A-TEST-1 FAIL"
               DISPLAY "  A=" WS-A OF WS-DST
               DISPLAY "  B=" WS-B OF WS-DST
           END-IF.
      * Test 2: Non-matching field unchanged
      *   WS-D is only in DST, should be unchanged
           IF WS-D OF WS-DST = 999
               DISPLAY "NC262A-TEST-2 PASS"
           ELSE
               DISPLAY "NC262A-TEST-2 FAIL"
               DISPLAY "  D should be 999, got "
                   WS-D OF WS-DST
           END-IF.
      * Test 3: Source fields unchanged after ADD CORR
           IF WS-A OF WS-SRC = 100
               AND WS-B OF WS-SRC = 200
               DISPLAY "NC262A-TEST-3 PASS"
           ELSE
               DISPLAY "NC262A-TEST-3 FAIL"
               DISPLAY "  SRC A=" WS-A OF WS-SRC
                   " B=" WS-B OF WS-SRC
           END-IF.
           STOP RUN.
