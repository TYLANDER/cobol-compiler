       IDENTIFICATION DIVISION.
       PROGRAM-ID. RL103A.
      *
      * NIST CCVS-style: Relative File — Multiple Records
      * Write many records sequentially, verify count.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REL-FILE ASSIGN TO "/tmp/RL103A.dat"
             ORGANIZATION IS RELATIVE
             ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD REL-FILE.
       01 REL-RECORD.
          05 REL-NUM PIC 9(4).
          05 REL-DATA PIC X(16).
       WORKING-STORAGE SECTION.
       01 WS-RECORD.
          05 WS-NUM PIC 9(4).
          05 WS-DATA PIC X(16).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-COUNT PIC 9(4) VALUE 0.
       01 WS-IDX PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Write 20 records
           OPEN OUTPUT REL-FILE.
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > 20
               MOVE WS-IDX TO REL-NUM
               MOVE "TEST-DATA       " TO REL-DATA
               WRITE REL-RECORD
           END-PERFORM.
           CLOSE REL-FILE.
      * TEST-1: Verify first record
           OPEN INPUT REL-FILE.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-NUM = 1
               DISPLAY "RL103A-TEST-1 PASS"
           ELSE
               DISPLAY "RL103A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Count all records
           MOVE 1 TO WS-COUNT.
           PERFORM UNTIL WS-EOF = 1
               READ REL-FILE INTO WS-RECORD
                 AT END MOVE 1 TO WS-EOF
               END-READ
               IF WS-EOF = 0
                   ADD 1 TO WS-COUNT
               END-IF
           END-PERFORM.
           CLOSE REL-FILE.
           IF WS-COUNT = 20
               DISPLAY "RL103A-TEST-2 PASS"
           ELSE
               DISPLAY "RL103A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Verify last record read was #20
           IF WS-NUM = 20
               DISPLAY "RL103A-TEST-3 PASS"
           ELSE
               DISPLAY "RL103A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
