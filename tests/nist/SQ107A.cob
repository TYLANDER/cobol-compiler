       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ107A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests writing multiple records and reading them back
      * sequentially, verifying order and content.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ107A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(40).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(40).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-ALL-MATCH PIC 9 VALUE 1.
       PROCEDURE DIVISION.
      * Write 5 records with different content
           OPEN OUTPUT SEQ-FILE.
           MOVE "RECORD-01 FIRST CONTENT" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "RECORD-02 SECOND CONTENT" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "RECORD-03 THIRD CONTENT" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "RECORD-04 FOURTH CONTENT" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "RECORD-05 FIFTH CONTENT" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Test 1: Read first record and verify content
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:23) = "RECORD-01 FIRST CONTENT"
               DISPLAY "SQ107A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ107A-TEST-1 FAIL"
           END-IF.
      * Test 2: Read remaining records and verify order
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:24) NOT =
              "RECORD-02 SECOND CONTENT"
               MOVE 0 TO WS-ALL-MATCH
           END-IF.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:23) NOT =
              "RECORD-03 THIRD CONTENT"
               MOVE 0 TO WS-ALL-MATCH
           END-IF.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:24) NOT =
              "RECORD-04 FOURTH CONTENT"
               MOVE 0 TO WS-ALL-MATCH
           END-IF.
           IF WS-ALL-MATCH = 1
               DISPLAY "SQ107A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ107A-TEST-2 FAIL"
           END-IF.
      * Test 3: Read fifth record and confirm EOF after
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:24) = "RECORD-05 FIFTH CONTENT"
               READ SEQ-FILE INTO WS-RECORD
                 AT END MOVE 1 TO WS-EOF
               END-READ
               IF WS-EOF = 1
                   DISPLAY "SQ107A-TEST-3 PASS"
               ELSE
                   DISPLAY "SQ107A-TEST-3 FAIL"
               END-IF
           ELSE
               DISPLAY "SQ107A-TEST-3 FAIL"
           END-IF.
           CLOSE SEQ-FILE.
           STOP RUN.
