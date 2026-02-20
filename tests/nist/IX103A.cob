       IDENTIFICATION DIVISION.
       PROGRAM-ID. IX103A.
      *
      * NIST CCVS-style: Indexed File — Many Records
      * Write many records, verify count and data integrity.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN TO "/tmp/IX103A.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL
             RECORD KEY IS IDX-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD IDX-FILE.
       01 IDX-RECORD.
          05 IDX-KEY PIC 9(4).
          05 IDX-DATA PIC X(16).
       WORKING-STORAGE SECTION.
       01 WS-RECORD.
          05 WS-KEY PIC 9(4).
          05 WS-DATA PIC X(16).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-COUNT PIC 9(4) VALUE 0.
       01 WS-IDX PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Write 15 records
           OPEN OUTPUT IDX-FILE.
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > 15
               MOVE WS-IDX TO IDX-KEY
               MOVE "INDEXED-DATA    " TO IDX-DATA
               WRITE IDX-RECORD
           END-PERFORM.
           CLOSE IDX-FILE.
      * TEST-1: Read first record
           MOVE 0 TO WS-EOF.
           OPEN INPUT IDX-FILE.
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-KEY = 0001
               DISPLAY "IX103A-TEST-1 PASS"
           ELSE
               DISPLAY "IX103A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Count all records
           MOVE 1 TO WS-COUNT.
           PERFORM UNTIL WS-EOF = 1
               READ IDX-FILE INTO WS-RECORD
                 AT END MOVE 1 TO WS-EOF
               END-READ
               IF WS-EOF = 0
                   ADD 1 TO WS-COUNT
               END-IF
           END-PERFORM.
           CLOSE IDX-FILE.
           IF WS-COUNT = 15
               DISPLAY "IX103A-TEST-2 PASS"
           ELSE
               DISPLAY "IX103A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Verify last record
           IF WS-KEY = 0015
               DISPLAY "IX103A-TEST-3 PASS"
           ELSE
               DISPLAY "IX103A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
