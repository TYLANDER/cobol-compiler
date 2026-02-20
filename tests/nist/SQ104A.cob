       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ104A.
      *
      * NIST CCVS-style test: Relative File I/O
      * Tests REWRITE statement (OPEN I-O, READ, REWRITE).
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REL-FILE ASSIGN TO "/tmp/SQ104A.dat"
             ORGANIZATION IS RELATIVE
             ACCESS MODE IS SEQUENTIAL
             RELATIVE KEY IS WS-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD REL-FILE.
       01 REL-RECORD PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-KEY PIC 9(4) VALUE 0.
       01 WS-RESULT PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-PASS-COUNT PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write 3 records
           OPEN OUTPUT REL-FILE.
           MOVE "ORIGINAL-REC-1" TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "ORIGINAL-REC-2" TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "ORIGINAL-REC-3" TO REL-RECORD.
           WRITE REL-RECORD.
           CLOSE REL-FILE.
      * Rewrite record 1
           OPEN I-O REL-FILE.
           READ REL-FILE INTO WS-RESULT
             AT END MOVE 1 TO WS-EOF.
           MOVE "UPDATED-REC-1" TO REL-RECORD.
           REWRITE REL-RECORD.
           CLOSE REL-FILE.
      * Read back and verify
           OPEN INPUT REL-FILE.
           READ REL-FILE INTO WS-RESULT
             AT END MOVE 1 TO WS-EOF.
           IF WS-RESULT(1:13) = "UPDATED-REC-1"
             ADD 1 TO WS-PASS-COUNT
           END-IF.
           READ REL-FILE INTO WS-RESULT
             AT END MOVE 1 TO WS-EOF.
           IF WS-RESULT(1:14) = "ORIGINAL-REC-2"
             ADD 1 TO WS-PASS-COUNT
           END-IF.
           READ REL-FILE INTO WS-RESULT
             AT END MOVE 1 TO WS-EOF.
           IF WS-RESULT(1:14) = "ORIGINAL-REC-3"
             ADD 1 TO WS-PASS-COUNT
           END-IF.
           CLOSE REL-FILE.
           IF WS-PASS-COUNT = 3
             DISPLAY "SQ104A-TEST-1 PASS"
           ELSE
             DISPLAY "SQ104A-TEST-1 FAIL"
           END-IF.
           STOP RUN.
