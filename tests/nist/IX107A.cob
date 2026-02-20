       IDENTIFICATION DIVISION.
       PROGRAM-ID. IX107A.
      *
      * NIST CCVS-style: Indexed File — OPEN EXTEND
      * Append records to an indexed file using EXTEND mode.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN TO "/tmp/IX107A.dat"
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
       01 WS-COUNT PIC 99 VALUE 0.
       PROCEDURE DIVISION.
      * Write initial records
           OPEN OUTPUT IDX-FILE.
           MOVE "A001" TO IDX-KEY.
           MOVE "INITIAL-REC-1   " TO IDX-DATA.
           WRITE IDX-RECORD.
           MOVE "A002" TO IDX-KEY.
           MOVE "INITIAL-REC-2   " TO IDX-DATA.
           WRITE IDX-RECORD.
           CLOSE IDX-FILE.
      * Extend with more records
           OPEN EXTEND IDX-FILE.
           MOVE "A003" TO IDX-KEY.
           MOVE "EXTENDED-REC-3  " TO IDX-DATA.
           WRITE IDX-RECORD.
           MOVE "A004" TO IDX-KEY.
           MOVE "EXTENDED-REC-4  " TO IDX-DATA.
           WRITE IDX-RECORD.
           CLOSE IDX-FILE.
      * TEST-1: Count all records
           MOVE 0 TO WS-EOF.
           MOVE 0 TO WS-COUNT.
           OPEN INPUT IDX-FILE.
           PERFORM UNTIL WS-EOF = 1
               READ IDX-FILE INTO WS-RECORD
                 AT END MOVE 1 TO WS-EOF
               END-READ
               IF WS-EOF = 0
                   ADD 1 TO WS-COUNT
               END-IF
           END-PERFORM.
           CLOSE IDX-FILE.
           IF WS-COUNT = 4
               DISPLAY "IX107A-TEST-1 PASS"
           ELSE
               DISPLAY "IX107A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Verify first record unchanged
           MOVE 0 TO WS-EOF.
           OPEN INPUT IDX-FILE.
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-DATA(1:13) = "INITIAL-REC-1"
               DISPLAY "IX107A-TEST-2 PASS"
           ELSE
               DISPLAY "IX107A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Verify third record was extended
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-DATA(1:14) = "EXTENDED-REC-3"
               DISPLAY "IX107A-TEST-3 PASS"
           ELSE
               DISPLAY "IX107A-TEST-3 FAIL"
           END-IF.
           CLOSE IDX-FILE.
           STOP RUN.
