       IDENTIFICATION DIVISION.
       PROGRAM-ID. RL104A.
      *
      * NIST CCVS-style: Relative File — START Positioning
      * Write records, use START to position, then read.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REL-FILE ASSIGN TO "/tmp/RL104A.dat"
             ORGANIZATION IS RELATIVE
             ACCESS MODE IS DYNAMIC
             RELATIVE KEY IS WS-REL-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD REL-FILE.
       01 REL-RECORD PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-REL-KEY PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Write 5 records
           OPEN OUTPUT REL-FILE.
           MOVE "FIRST-RECORD        " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "SECOND-RECORD       " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "THIRD-RECORD        " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "FOURTH-RECORD       " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "FIFTH-RECORD        " TO REL-RECORD.
           WRITE REL-RECORD.
           CLOSE REL-FILE.
      * TEST-1: START at record 3, read it
           OPEN INPUT REL-FILE.
           MOVE 3 TO WS-REL-KEY.
           START REL-FILE.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:12) = "THIRD-RECORD"
               DISPLAY "RL104A-TEST-1 PASS"
           ELSE
               DISPLAY "RL104A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Continue sequential read to get record 4
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:13) = "FOURTH-RECORD"
               DISPLAY "RL104A-TEST-2 PASS"
           ELSE
               DISPLAY "RL104A-TEST-2 FAIL"
           END-IF.
      * TEST-3: START at record 1, verify
           MOVE 1 TO WS-REL-KEY.
           START REL-FILE.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:12) = "FIRST-RECORD"
               DISPLAY "RL104A-TEST-3 PASS"
           ELSE
               DISPLAY "RL104A-TEST-3 FAIL"
           END-IF.
           CLOSE REL-FILE.
           STOP RUN.
