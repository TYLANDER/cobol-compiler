       IDENTIFICATION DIVISION.
       PROGRAM-ID. IX102A.
      *
      * NIST CCVS-style: Indexed File — REWRITE
      * Write records, rewrite one, verify update.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN TO "/tmp/IX102A.dat"
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
      * Write 3 records
           OPEN OUTPUT IDX-FILE.
           MOVE "K001" TO IDX-KEY.
           MOVE "ORIGINAL-DATA-01" TO IDX-DATA.
           WRITE IDX-RECORD.
           MOVE "K002" TO IDX-KEY.
           MOVE "ORIGINAL-DATA-02" TO IDX-DATA.
           WRITE IDX-RECORD.
           MOVE "K003" TO IDX-KEY.
           MOVE "ORIGINAL-DATA-03" TO IDX-DATA.
           WRITE IDX-RECORD.
           CLOSE IDX-FILE.
      * Rewrite first record
           OPEN I-O IDX-FILE.
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           MOVE "UPDATED--DATA-01" TO IDX-DATA.
           REWRITE IDX-RECORD.
           CLOSE IDX-FILE.
      * TEST-1: Verify updated record
           MOVE 0 TO WS-EOF.
           OPEN INPUT IDX-FILE.
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-DATA(1:16) = "UPDATED--DATA-01"
               DISPLAY "IX102A-TEST-1 PASS"
           ELSE
               DISPLAY "IX102A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Verify second record unchanged
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-DATA(1:16) = "ORIGINAL-DATA-02"
               DISPLAY "IX102A-TEST-2 PASS"
           ELSE
               DISPLAY "IX102A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Verify third record unchanged
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-DATA(1:16) = "ORIGINAL-DATA-03"
               DISPLAY "IX102A-TEST-3 PASS"
           ELSE
               DISPLAY "IX102A-TEST-3 FAIL"
           END-IF.
           CLOSE IDX-FILE.
           STOP RUN.
