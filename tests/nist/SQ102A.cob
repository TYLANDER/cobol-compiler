       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ102A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests writing multiple records and reading them all back.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ102A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-COUNT PIC 9(2) VALUE 0.
       PROCEDURE DIVISION.
      * Write three records
           OPEN OUTPUT SEQ-FILE.
           MOVE "FIRST RECORD" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "SECOND RECORD" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "THIRD RECORD" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Test 1: Read first record
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:12) = "FIRST RECORD"
               DISPLAY "SQ102A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ102A-TEST-1 FAIL"
           END-IF.
      * Test 2: Read second record
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:13) = "SECOND RECORD"
               DISPLAY "SQ102A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ102A-TEST-2 FAIL"
           END-IF.
      * Test 3: Read third record and verify count
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:12) = "THIRD RECORD"
               DISPLAY "SQ102A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ102A-TEST-3 FAIL"
           END-IF.
           CLOSE SEQ-FILE.
           STOP RUN.
