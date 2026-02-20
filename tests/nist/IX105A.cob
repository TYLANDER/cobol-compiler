       IDENTIFICATION DIVISION.
       PROGRAM-ID. IX105A.
      *
      * NIST CCVS-style: Indexed File — AT END Detection
      * Tests EOF handling with indexed files.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN TO "/tmp/IX105A.dat"
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
      * TEST-1: Empty file triggers AT END
           OPEN OUTPUT IDX-FILE.
           CLOSE IDX-FILE.
           OPEN INPUT IDX-FILE.
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           CLOSE IDX-FILE.
           IF WS-EOF = 1
               DISPLAY "IX105A-TEST-1 PASS"
           ELSE
               DISPLAY "IX105A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Single record, read past triggers AT END
           MOVE 0 TO WS-EOF.
           OPEN OUTPUT IDX-FILE.
           MOVE "K001" TO IDX-KEY.
           MOVE "SINGLE RECORD   " TO IDX-DATA.
           WRITE IDX-RECORD.
           CLOSE IDX-FILE.
           OPEN INPUT IDX-FILE.
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-EOF = 0
               DISPLAY "IX105A-TEST-2 PASS"
           ELSE
               DISPLAY "IX105A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Second read on single-record file
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           CLOSE IDX-FILE.
           IF WS-EOF = 1
               DISPLAY "IX105A-TEST-3 PASS"
           ELSE
               DISPLAY "IX105A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
