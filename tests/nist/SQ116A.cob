       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ116A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Write 5 records, read them back, verify record 3 content.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ116A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(40).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(40).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       01 WS-REC3 PIC X(40) VALUE SPACES.
       01 WS-REC5 PIC X(40) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write 5 records
           OPEN OUTPUT SEQ-FILE.
           MOVE "ALPHA RECORD ONE" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "BETA RECORD TWO" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "GAMMA RECORD THREE" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "DELTA RECORD FOUR" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "EPSILON RECORD FIVE" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Read records back and capture record 3
           OPEN INPUT SEQ-FILE.
           MOVE 0 TO WS-EOF.
           MOVE 0 TO WS-READ-COUNT.
           PERFORM UNTIL WS-EOF = 1
               READ SEQ-FILE INTO WS-RECORD
                 AT END MOVE 1 TO WS-EOF
               END-READ
               IF WS-EOF = 0
                   ADD 1 TO WS-READ-COUNT
                   IF WS-READ-COUNT = 3
                       MOVE WS-RECORD TO WS-REC3
                   END-IF
                   IF WS-READ-COUNT = 5
                       MOVE WS-RECORD TO WS-REC5
                   END-IF
               END-IF
           END-PERFORM.
           CLOSE SEQ-FILE.
      * Test 1: Verify total record count is 5
           IF WS-READ-COUNT = 5
               DISPLAY "SQ116A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ116A-TEST-1 FAIL"
           END-IF.
      * Test 2: Verify record 3 content
           IF WS-REC3 = "GAMMA RECORD THREE"
               DISPLAY "SQ116A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ116A-TEST-2 FAIL"
           END-IF.
      * Test 3: Verify record 5 content
           IF WS-REC5 = "EPSILON RECORD FIVE"
               DISPLAY "SQ116A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ116A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
