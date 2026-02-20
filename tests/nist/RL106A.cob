       IDENTIFICATION DIVISION.
       PROGRAM-ID. RL106A.
      *
      * NIST CCVS-style: Relative File — Structured Records
      * Tests relative files with multi-field records.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REL-FILE ASSIGN TO "/tmp/RL106A.dat"
             ORGANIZATION IS RELATIVE
             ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD REL-FILE.
       01 REL-RECORD.
          05 REL-ID PIC 9(4).
          05 REL-NAME PIC X(10).
          05 REL-AMOUNT PIC 9(4)V99.
       WORKING-STORAGE SECTION.
       01 WS-RECORD.
          05 WS-ID PIC 9(4).
          05 WS-NAME PIC X(10).
          05 WS-AMOUNT PIC 9(4)V99.
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write structured records
           OPEN OUTPUT REL-FILE.
           MOVE 0001 TO REL-ID.
           MOVE "SMITH     " TO REL-NAME.
           MOVE 1500.75 TO REL-AMOUNT.
           WRITE REL-RECORD.
           MOVE 0002 TO REL-ID.
           MOVE "JONES     " TO REL-NAME.
           MOVE 2300.50 TO REL-AMOUNT.
           WRITE REL-RECORD.
           MOVE 0003 TO REL-ID.
           MOVE "WILLIAMS  " TO REL-NAME.
           MOVE 0875.25 TO REL-AMOUNT.
           WRITE REL-RECORD.
           CLOSE REL-FILE.
      * TEST-1: Read first record and verify ID
           OPEN INPUT REL-FILE.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-ID = 0001
               DISPLAY "RL106A-TEST-1 PASS"
           ELSE
               DISPLAY "RL106A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Verify name field
           IF WS-NAME = "SMITH     "
               DISPLAY "RL106A-TEST-2 PASS"
           ELSE
               DISPLAY "RL106A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Read and verify second record
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-ID = 0002
               DISPLAY "RL106A-TEST-3 PASS"
           ELSE
               DISPLAY "RL106A-TEST-3 FAIL"
           END-IF.
      * TEST-4: Verify third record amount
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-AMOUNT = 875.25
               DISPLAY "RL106A-TEST-4 PASS"
           ELSE
               DISPLAY "RL106A-TEST-4 FAIL"
           END-IF.
           CLOSE REL-FILE.
           STOP RUN.
