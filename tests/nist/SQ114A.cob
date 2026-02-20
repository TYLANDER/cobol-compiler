       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ114A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests CLOSE and reopen the same file.
      * Writes records, closes, reopens for INPUT, reads
      * back and verifies the data is intact.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ114A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(30).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(30) VALUE SPACES.
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write records to the file
           OPEN OUTPUT SEQ-FILE.
           MOVE "CLOSE-REOPEN REC ONE" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "CLOSE-REOPEN REC TWO" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "CLOSE-REOPEN REC THREE" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Test 1: Reopen for INPUT and read first record
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:20) = "CLOSE-REOPEN REC ONE"
               DISPLAY "SQ114A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ114A-TEST-1 FAIL"
           END-IF.
           CLOSE SEQ-FILE.
      * Test 2: Reopen again for INPUT, read starts from top
           MOVE SPACES TO WS-RECORD.
           MOVE 0 TO WS-EOF.
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:20) = "CLOSE-REOPEN REC ONE"
               DISPLAY "SQ114A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ114A-TEST-2 FAIL"
           END-IF.
      * Test 3: Read remaining records to verify all intact
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:20) NOT =
              "CLOSE-REOPEN REC TWO"
               DISPLAY "SQ114A-TEST-3 FAIL"
               CLOSE SEQ-FILE
               STOP RUN
           END-IF.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:22) =
              "CLOSE-REOPEN REC THREE"
               DISPLAY "SQ114A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ114A-TEST-3 FAIL"
           END-IF.
           CLOSE SEQ-FILE.
           STOP RUN.
