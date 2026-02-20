       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ108A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests write multiple records, read them back in order,
      * and verify content matches.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ108A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(30).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(30).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write three records to the file
           OPEN OUTPUT SEQ-FILE.
           MOVE "RECORD-ALPHA" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "RECORD-BETA" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "RECORD-GAMMA" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Read back and verify
           OPEN INPUT SEQ-FILE.
      * Test 1: First record is ALPHA
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD = "RECORD-ALPHA"
               DISPLAY "SQ108A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ108A-TEST-1 FAIL"
           END-IF.
      * Test 2: Second record is BETA
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD = "RECORD-BETA"
               DISPLAY "SQ108A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ108A-TEST-2 FAIL"
           END-IF.
      * Test 3: Third record is GAMMA
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD = "RECORD-GAMMA"
               DISPLAY "SQ108A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ108A-TEST-3 FAIL"
           END-IF.
           CLOSE SEQ-FILE.
           STOP RUN.
