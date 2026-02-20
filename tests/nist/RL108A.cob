       IDENTIFICATION DIVISION.
       PROGRAM-ID. RL108A.
      *
      * NIST CCVS-style: Relative File — OPEN EXTEND
      * Append records using OPEN EXTEND mode.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REL-FILE ASSIGN TO "/tmp/RL108A.dat"
             ORGANIZATION IS RELATIVE
             ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD REL-FILE.
       01 REL-RECORD PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-COUNT PIC 99 VALUE 0.
       PROCEDURE DIVISION.
      * Write 2 initial records
           OPEN OUTPUT REL-FILE.
           MOVE "INITIAL-REC-1       " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "INITIAL-REC-2       " TO REL-RECORD.
           WRITE REL-RECORD.
           CLOSE REL-FILE.
      * Append 2 more records with EXTEND
           OPEN EXTEND REL-FILE.
           MOVE "EXTENDED-REC-3      " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "EXTENDED-REC-4      " TO REL-RECORD.
           WRITE REL-RECORD.
           CLOSE REL-FILE.
      * TEST-1: Count all records — should be 4
           MOVE 0 TO WS-EOF.
           MOVE 0 TO WS-COUNT.
           OPEN INPUT REL-FILE.
           PERFORM UNTIL WS-EOF = 1
               READ REL-FILE INTO WS-RECORD
                 AT END MOVE 1 TO WS-EOF
               END-READ
               IF WS-EOF = 0
                   ADD 1 TO WS-COUNT
               END-IF
           END-PERFORM.
           CLOSE REL-FILE.
           IF WS-COUNT = 4
               DISPLAY "RL108A-TEST-1 PASS"
           ELSE
               DISPLAY "RL108A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Re-read and verify first record
           MOVE 0 TO WS-EOF.
           OPEN INPUT REL-FILE.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:13) = "INITIAL-REC-1"
               DISPLAY "RL108A-TEST-2 PASS"
           ELSE
               DISPLAY "RL108A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Skip to third record and verify it was appended
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:14) = "EXTENDED-REC-3"
               DISPLAY "RL108A-TEST-3 PASS"
           ELSE
               DISPLAY "RL108A-TEST-3 FAIL"
           END-IF.
           CLOSE REL-FILE.
           STOP RUN.
