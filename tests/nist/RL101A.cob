       IDENTIFICATION DIVISION.
       PROGRAM-ID. RL101A.
      *
      * NIST CCVS-style: Relative File — Sequential Write & Read
      * Write 5 records sequentially, read them back, verify content.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REL-FILE ASSIGN TO "/tmp/RL101A.dat"
             ORGANIZATION IS RELATIVE
             ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD REL-FILE.
       01 REL-RECORD PIC X(30).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(30).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-COUNT PIC 99 VALUE 0.
       PROCEDURE DIVISION.
      * TEST-1: Write 5 records
           OPEN OUTPUT REL-FILE.
           MOVE "RECORD-001 ALPHA DATA       " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "RECORD-002 BETA DATA        " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "RECORD-003 GAMMA DATA       " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "RECORD-004 DELTA DATA       " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "RECORD-005 EPSILON DATA     " TO REL-RECORD.
           WRITE REL-RECORD.
           CLOSE REL-FILE.
      * Read all records back
           MOVE 0 TO WS-EOF.
           MOVE 0 TO WS-COUNT.
           OPEN INPUT REL-FILE.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:10) = "RECORD-001"
               DISPLAY "RL101A-TEST-1 PASS"
           ELSE
               DISPLAY "RL101A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Second record
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:10) = "RECORD-002"
               DISPLAY "RL101A-TEST-2 PASS"
           ELSE
               DISPLAY "RL101A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Read remaining and count
           PERFORM UNTIL WS-EOF = 1
               READ REL-FILE INTO WS-RECORD
                 AT END MOVE 1 TO WS-EOF
               END-READ
               IF WS-EOF = 0
                   ADD 1 TO WS-COUNT
               END-IF
           END-PERFORM.
           CLOSE REL-FILE.
           IF WS-COUNT = 3
               DISPLAY "RL101A-TEST-3 PASS"
           ELSE
               DISPLAY "RL101A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
