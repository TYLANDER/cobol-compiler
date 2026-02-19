       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC316A.
      *
      * NIST CCVS-style test: ADD CORRESPONDING
      * Tests ADD CORRESPONDING between group items, verifying
      * that matching numeric fields are added correctly.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC-GRP.
           05 WS-AA        PIC 9(4) VALUE ZEROS.
           05 WS-BB        PIC 9(4) VALUE ZEROS.
           05 WS-CC        PIC 9(4) VALUE ZEROS.
       01 WS-DST-GRP.
           05 WS-AA        PIC 9(4) VALUE ZEROS.
           05 WS-BB        PIC 9(4) VALUE ZEROS.
           05 WS-CC        PIC 9(4) VALUE ZEROS.
           05 WS-DD        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: ADD CORRESPONDING basic
      *   SRC: AA=10, BB=20, CC=30
      *   DST: AA=100, BB=200, CC=300, DD=400
      *   After: DST AA=110, BB=220, CC=330, DD=400 (unchanged)
           MOVE 10 TO WS-AA OF WS-SRC-GRP.
           MOVE 20 TO WS-BB OF WS-SRC-GRP.
           MOVE 30 TO WS-CC OF WS-SRC-GRP.
           MOVE 100 TO WS-AA OF WS-DST-GRP.
           MOVE 200 TO WS-BB OF WS-DST-GRP.
           MOVE 300 TO WS-CC OF WS-DST-GRP.
           MOVE 400 TO WS-DD.
           ADD CORRESPONDING WS-SRC-GRP TO WS-DST-GRP.
           IF WS-AA OF WS-DST-GRP = 110
               AND WS-BB OF WS-DST-GRP = 220
               AND WS-CC OF WS-DST-GRP = 330
               DISPLAY "NC316A-TEST-1 PASS"
           ELSE
               DISPLAY "NC316A-TEST-1 FAIL"
               DISPLAY "  Expected AA=110 BB=220 CC=330"
               DISPLAY "  Got AA=" WS-AA OF WS-DST-GRP
                   " BB=" WS-BB OF WS-DST-GRP
                   " CC=" WS-CC OF WS-DST-GRP
           END-IF.
      * Test 2: Non-matching field DD is unchanged
           IF WS-DD = 400
               DISPLAY "NC316A-TEST-2 PASS"
           ELSE
               DISPLAY "NC316A-TEST-2 FAIL"
               DISPLAY "  Expected DD=400, got " WS-DD
           END-IF.
      * Test 3: Source fields unchanged after ADD CORRESPONDING
           IF WS-AA OF WS-SRC-GRP = 10
               AND WS-BB OF WS-SRC-GRP = 20
               AND WS-CC OF WS-SRC-GRP = 30
               DISPLAY "NC316A-TEST-3 PASS"
           ELSE
               DISPLAY "NC316A-TEST-3 FAIL"
               DISPLAY "  Source modified"
               DISPLAY "  Got AA=" WS-AA OF WS-SRC-GRP
                   " BB=" WS-BB OF WS-SRC-GRP
                   " CC=" WS-CC OF WS-SRC-GRP
           END-IF.
           STOP RUN.
