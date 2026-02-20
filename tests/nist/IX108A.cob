       IDENTIFICATION DIVISION.
       PROGRAM-ID. IX108A.
      *
      * NIST CCVS-style: Indexed File — START Positioning
      * Write records, use START to position for reading.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN TO "/tmp/IX108A.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS IDX-KEY
             RELATIVE KEY IS WS-REL-KEY.
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
       01 WS-REL-KEY PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Write 5 records
           OPEN OUTPUT IDX-FILE.
           MOVE "K001" TO IDX-KEY.
           MOVE "FIRST-DATA      " TO IDX-DATA.
           WRITE IDX-RECORD.
           MOVE "K002" TO IDX-KEY.
           MOVE "SECOND-DATA     " TO IDX-DATA.
           WRITE IDX-RECORD.
           MOVE "K003" TO IDX-KEY.
           MOVE "THIRD-DATA      " TO IDX-DATA.
           WRITE IDX-RECORD.
           MOVE "K004" TO IDX-KEY.
           MOVE "FOURTH-DATA     " TO IDX-DATA.
           WRITE IDX-RECORD.
           MOVE "K005" TO IDX-KEY.
           MOVE "FIFTH-DATA      " TO IDX-DATA.
           WRITE IDX-RECORD.
           CLOSE IDX-FILE.
      * TEST-1: START at record 3
           OPEN INPUT IDX-FILE.
           MOVE 3 TO WS-REL-KEY.
           START IDX-FILE.
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-KEY = "K003"
               DISPLAY "IX108A-TEST-1 PASS"
           ELSE
               DISPLAY "IX108A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Sequential read after START
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-KEY = "K004"
               DISPLAY "IX108A-TEST-2 PASS"
           ELSE
               DISPLAY "IX108A-TEST-2 FAIL"
           END-IF.
      * TEST-3: START at record 1
           MOVE 1 TO WS-REL-KEY.
           START IDX-FILE.
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-KEY = "K001"
               DISPLAY "IX108A-TEST-3 PASS"
           ELSE
               DISPLAY "IX108A-TEST-3 FAIL"
           END-IF.
           CLOSE IDX-FILE.
           STOP RUN.
