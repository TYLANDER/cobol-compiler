       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-RELATIVE.
      *
      * Smoke test: Relative file I/O
      * Tests basic write/read cycle with fixed-length records.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REL-FILE ASSIGN TO "/tmp/SMOKE-REL.dat"
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
      * Write 3 records sequentially
           OPEN OUTPUT REL-FILE.
           MOVE "RECORD ONE          " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "RECORD TWO          " TO REL-RECORD.
           WRITE REL-RECORD.
           MOVE "RECORD THREE        " TO REL-RECORD.
           WRITE REL-RECORD.
           CLOSE REL-FILE.
      * Read them back sequentially
           MOVE 0 TO WS-EOF.
           OPEN INPUT REL-FILE.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:10) = "RECORD ONE"
               DISPLAY "TEST-1 PASS"
           ELSE
               DISPLAY "TEST-1 FAIL"
           END-IF.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:10) = "RECORD TWO"
               DISPLAY "TEST-2 PASS"
           ELSE
               DISPLAY "TEST-2 FAIL"
           END-IF.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:12) = "RECORD THREE"
               DISPLAY "TEST-3 PASS"
           ELSE
               DISPLAY "TEST-3 FAIL"
           END-IF.
      * Verify AT END on 4th read
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-EOF = 1
               DISPLAY "TEST-4 PASS"
           ELSE
               DISPLAY "TEST-4 FAIL"
           END-IF.
           CLOSE REL-FILE.
           STOP RUN.
