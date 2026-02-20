       IDENTIFICATION DIVISION.
       PROGRAM-ID. IX106A.
      *
      * NIST CCVS-style: Indexed File — Multiple Rewrites
      * Write records, rewrite several, verify all changes.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN TO "/tmp/IX106A.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL
             RECORD KEY IS IDX-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD IDX-FILE.
       01 IDX-RECORD.
          05 IDX-KEY PIC X(4).
          05 IDX-DATA PIC X(16).
       WORKING-STORAGE SECTION.
       01 WS-RECORD.
          05 WS-KEY PIC X(4).
          05 WS-DATA PIC X(16).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write 4 records
           OPEN OUTPUT IDX-FILE.
           MOVE "K001" TO IDX-KEY.
           MOVE "ALPHA-ORIGINAL  " TO IDX-DATA.
           WRITE IDX-RECORD.
           MOVE "K002" TO IDX-KEY.
           MOVE "BRAVO-ORIGINAL  " TO IDX-DATA.
           WRITE IDX-RECORD.
           MOVE "K003" TO IDX-KEY.
           MOVE "CHARLIE-ORIGINAL" TO IDX-DATA.
           WRITE IDX-RECORD.
           MOVE "K004" TO IDX-KEY.
           MOVE "DELTA-ORIGINAL  " TO IDX-DATA.
           WRITE IDX-RECORD.
           CLOSE IDX-FILE.
      * Rewrite records 1 and 3
           OPEN I-O IDX-FILE.
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           MOVE "ALPHA-UPDATED   " TO IDX-DATA.
           REWRITE IDX-RECORD.
      * Skip record 2
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
      * Read and rewrite record 3
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           MOVE "CHARLIE-UPDATED " TO IDX-DATA.
           REWRITE IDX-RECORD.
           CLOSE IDX-FILE.
      * Verify all records
           MOVE 0 TO WS-EOF.
           OPEN INPUT IDX-FILE.
      * TEST-1: Record 1 updated
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-DATA(1:13) = "ALPHA-UPDATED"
               DISPLAY "IX106A-TEST-1 PASS"
           ELSE
               DISPLAY "IX106A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Record 2 unchanged
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-DATA(1:14) = "BRAVO-ORIGINAL"
               DISPLAY "IX106A-TEST-2 PASS"
           ELSE
               DISPLAY "IX106A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Record 3 updated
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-DATA(1:15) = "CHARLIE-UPDATED"
               DISPLAY "IX106A-TEST-3 PASS"
           ELSE
               DISPLAY "IX106A-TEST-3 FAIL"
           END-IF.
      * TEST-4: Record 4 unchanged
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-DATA(1:14) = "DELTA-ORIGINAL"
               DISPLAY "IX106A-TEST-4 PASS"
           ELSE
               DISPLAY "IX106A-TEST-4 FAIL"
           END-IF.
           CLOSE IDX-FILE.
           STOP RUN.
