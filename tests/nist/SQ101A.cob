       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ101A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests basic OPEN/WRITE/CLOSE/OPEN/READ/CLOSE cycle.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ101A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Write a record and read it back
           OPEN OUTPUT SEQ-FILE.
           MOVE "HELLO SEQUENTIAL" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           CLOSE SEQ-FILE.
           IF WS-RECORD(1:16) = "HELLO SEQUENTIAL"
               DISPLAY "SQ101A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ101A-TEST-1 FAIL"
           END-IF.
      * Test 2: Write a different record and verify
           MOVE 0 TO WS-EOF.
           MOVE SPACES TO WS-RECORD.
           OPEN OUTPUT SEQ-FILE.
           MOVE "RECORD TWO DATA " TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           CLOSE SEQ-FILE.
           IF WS-RECORD(1:15) = "RECORD TWO DATA"
               DISPLAY "SQ101A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ101A-TEST-2 FAIL"
           END-IF.
      * Test 3: Verify AT END is triggered on empty file
           MOVE 0 TO WS-EOF.
           OPEN OUTPUT SEQ-FILE.
           CLOSE SEQ-FILE.
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           CLOSE SEQ-FILE.
           IF WS-EOF = 1
               DISPLAY "SQ101A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ101A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
