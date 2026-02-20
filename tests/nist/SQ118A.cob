       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ118A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Write records with different logical lengths but padded
      * to PIC X(40). Verify space padding on read-back.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ118A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(40).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(40).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-SHORT-DATA PIC X(5).
       01 WS-MEDIUM-DATA PIC X(20).
       01 WS-LONG-DATA PIC X(40).
       01 WS-REC1 PIC X(40) VALUE SPACES.
       01 WS-REC2 PIC X(40) VALUE SPACES.
       01 WS-REC3 PIC X(40) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write 3 records with different content lengths
           OPEN OUTPUT SEQ-FILE.
           MOVE SPACES TO SEQ-RECORD.
           MOVE "SHORT" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE SPACES TO SEQ-RECORD.
           MOVE "MEDIUM LENGTH RECORD" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE SPACES TO SEQ-RECORD.
           MOVE "THIS IS A FULL FORTY CHARACTER RECORD!!"
             TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Read back
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-REC1
             AT END MOVE 1 TO WS-EOF
           END-READ.
           READ SEQ-FILE INTO WS-REC2
             AT END MOVE 1 TO WS-EOF
           END-READ.
           READ SEQ-FILE INTO WS-REC3
             AT END MOVE 1 TO WS-EOF
           END-READ.
           CLOSE SEQ-FILE.
      * Test 1: Short record is space-padded on the right
           IF WS-REC1 = "SHORT"
               DISPLAY "SQ118A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ118A-TEST-1 FAIL"
           END-IF.
      * Test 2: Medium record read back correctly
           IF WS-REC2 = "MEDIUM LENGTH RECORD"
               DISPLAY "SQ118A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ118A-TEST-2 FAIL"
           END-IF.
      * Test 3: Full-length record read back correctly
           IF WS-REC3 =
              "THIS IS A FULL FORTY CHARACTER RECORD!!"
               DISPLAY "SQ118A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ118A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
