       IDENTIFICATION DIVISION.
       PROGRAM-ID. RL105A.
      *
      * NIST CCVS-style: Relative File — OPEN I-O Sequential
      * Open in I-O mode, read records, rewrite selected ones.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REL-FILE ASSIGN TO "/tmp/RL105A.dat"
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
      * Setup: Write 3 records
           OPEN OUTPUT REL-FILE.
           MOVE "AAA-ORIGINAL        " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "BBB-ORIGINAL        " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "CCC-ORIGINAL        " TO REL-RECORD.
           WRITE REL-RECORD.
           CLOSE REL-FILE.
      * TEST-1: I-O mode, read record 1 and rewrite
           OPEN I-O REL-FILE.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           MOVE "AAA-UPDATED         " TO REL-RECORD.
           REWRITE REL-RECORD.
      * Skip record 2, read record 3
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           MOVE "CCC-UPDATED         " TO REL-RECORD.
           REWRITE REL-RECORD.
           CLOSE REL-FILE.
      * Verify: read all records back
           MOVE 0 TO WS-EOF.
           OPEN INPUT REL-FILE.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:11) = "AAA-UPDATED"
               DISPLAY "RL105A-TEST-1 PASS"
           ELSE
               DISPLAY "RL105A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Record 2 should be unchanged
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:12) = "BBB-ORIGINAL"
               DISPLAY "RL105A-TEST-2 PASS"
           ELSE
               DISPLAY "RL105A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Record 3 should be updated
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:11) = "CCC-UPDATED"
               DISPLAY "RL105A-TEST-3 PASS"
           ELSE
               DISPLAY "RL105A-TEST-3 FAIL"
           END-IF.
           CLOSE REL-FILE.
           STOP RUN.
