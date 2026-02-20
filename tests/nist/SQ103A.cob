       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ103A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests WRITE with ADVANCING and FROM clauses.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ103A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(40).
       WORKING-STORAGE SECTION.
       01 WS-REC-1 PIC X(40) VALUE "RECORD-ONE".
       01 WS-REC-2 PIC X(40) VALUE "RECORD-TWO".
       01 WS-REC-3 PIC X(40) VALUE "RECORD-THREE".
       01 WS-RESULT PIC X(40).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-PASS-COUNT PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: WRITE FROM copies data into the record area
           OPEN OUTPUT SEQ-FILE.
           WRITE SEQ-RECORD FROM WS-REC-1.
           WRITE SEQ-RECORD FROM WS-REC-2.
           WRITE SEQ-RECORD FROM WS-REC-3.
           CLOSE SEQ-FILE.
      * Read back and verify FROM clause copied correctly
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-RESULT
             AT END MOVE 1 TO WS-EOF.
           IF WS-RESULT(1:10) = "RECORD-ONE"
             ADD 1 TO WS-PASS-COUNT
           END-IF.
           READ SEQ-FILE INTO WS-RESULT
             AT END MOVE 1 TO WS-EOF.
           IF WS-RESULT(1:10) = "RECORD-TWO"
             ADD 1 TO WS-PASS-COUNT
           END-IF.
           READ SEQ-FILE INTO WS-RESULT
             AT END MOVE 1 TO WS-EOF.
           IF WS-RESULT(1:13) = "RECORD-THREE"
             ADD 1 TO WS-PASS-COUNT
           END-IF.
           CLOSE SEQ-FILE.
      * Test 2: WRITE with ADVANCING (just verify no crash)
           OPEN OUTPUT SEQ-FILE.
           WRITE SEQ-RECORD FROM WS-REC-1
             AFTER ADVANCING 2 LINES.
           WRITE SEQ-RECORD FROM WS-REC-2
             BEFORE ADVANCING 1 LINE.
           CLOSE SEQ-FILE.
           ADD 1 TO WS-PASS-COUNT.
           IF WS-PASS-COUNT = 4
             DISPLAY "SQ103A-TEST-1 PASS"
           ELSE
             DISPLAY "SQ103A-TEST-1 FAIL"
           END-IF.
           STOP RUN.
