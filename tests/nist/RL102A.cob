       IDENTIFICATION DIVISION.
       PROGRAM-ID. RL102A.
      *
      * NIST CCVS-style: Relative File — REWRITE
      * Write records, rewrite one, verify the update.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REL-FILE ASSIGN TO "/tmp/RL102A.dat"
             ORGANIZATION IS RELATIVE
             ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD REL-FILE.
       01 REL-RECORD PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write 3 records
           OPEN OUTPUT REL-FILE.
           MOVE "ORIGINAL-REC-1      " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "ORIGINAL-REC-2      " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "ORIGINAL-REC-3      " TO REL-RECORD.
           WRITE REL-RECORD.
           CLOSE REL-FILE.
      * Open I-O, read record 1, rewrite it
           OPEN I-O REL-FILE.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           MOVE "UPDATED-REC-1       " TO REL-RECORD.
           REWRITE REL-RECORD.
           CLOSE REL-FILE.
      * TEST-1: Verify record 1 was updated
           MOVE 0 TO WS-EOF.
           OPEN INPUT REL-FILE.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:13) = "UPDATED-REC-1"
               DISPLAY "RL102A-TEST-1 PASS"
           ELSE
               DISPLAY "RL102A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Verify record 2 unchanged
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:14) = "ORIGINAL-REC-2"
               DISPLAY "RL102A-TEST-2 PASS"
           ELSE
               DISPLAY "RL102A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Verify record 3 unchanged
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:14) = "ORIGINAL-REC-3"
               DISPLAY "RL102A-TEST-3 PASS"
           ELSE
               DISPLAY "RL102A-TEST-3 FAIL"
           END-IF.
           CLOSE REL-FILE.
           STOP RUN.
