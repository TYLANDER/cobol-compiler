       IDENTIFICATION DIVISION.
       PROGRAM-ID. IX101A.
      *
      * NIST CCVS-style: Indexed File — Sequential Write & Read
      * Write records with keys, read them back sequentially.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN TO "/tmp/IX101A.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL
             RECORD KEY IS IDX-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD IDX-FILE.
       01 IDX-RECORD.
          05 IDX-KEY PIC X(5).
          05 IDX-DATA PIC X(15).
       WORKING-STORAGE SECTION.
       01 WS-RECORD.
          05 WS-KEY PIC X(5).
          05 WS-DATA PIC X(15).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write 3 records
           OPEN OUTPUT IDX-FILE.
           MOVE "KEY01" TO IDX-KEY.
           MOVE "FIRST RECORD   " TO IDX-DATA.
           WRITE IDX-RECORD.
           MOVE "KEY02" TO IDX-KEY.
           MOVE "SECOND RECORD  " TO IDX-DATA.
           WRITE IDX-RECORD.
           MOVE "KEY03" TO IDX-KEY.
           MOVE "THIRD RECORD   " TO IDX-DATA.
           WRITE IDX-RECORD.
           CLOSE IDX-FILE.
      * TEST-1: Read first record
           OPEN INPUT IDX-FILE.
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-KEY = "KEY01"
               DISPLAY "IX101A-TEST-1 PASS"
           ELSE
               DISPLAY "IX101A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Second record
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-KEY = "KEY02"
               DISPLAY "IX101A-TEST-2 PASS"
           ELSE
               DISPLAY "IX101A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Third record data
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-DATA(1:12) = "THIRD RECORD"
               DISPLAY "IX101A-TEST-3 PASS"
           ELSE
               DISPLAY "IX101A-TEST-3 FAIL"
           END-IF.
           CLOSE IDX-FILE.
           STOP RUN.
