       IDENTIFICATION DIVISION.
       PROGRAM-ID. IX104A.
      *
      * NIST CCVS-style: Indexed File — Structured Records
      * Tests indexed file with multi-field records.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN TO "/tmp/IX104A.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL
             RECORD KEY IS IDX-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD IDX-FILE.
       01 IDX-RECORD.
          05 IDX-KEY PIC 9(4).
          05 IDX-NAME PIC X(10).
          05 IDX-SALARY PIC 9(6)V99.
       WORKING-STORAGE SECTION.
       01 WS-RECORD.
          05 WS-KEY PIC 9(4).
          05 WS-NAME PIC X(10).
          05 WS-SALARY PIC 9(6)V99.
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write employee records
           OPEN OUTPUT IDX-FILE.
           MOVE 1001 TO IDX-KEY.
           MOVE "SMITH     " TO IDX-NAME.
           MOVE 045000.00 TO IDX-SALARY.
           WRITE IDX-RECORD.
           MOVE 1002 TO IDX-KEY.
           MOVE "JONES     " TO IDX-NAME.
           MOVE 052500.50 TO IDX-SALARY.
           WRITE IDX-RECORD.
           MOVE 1003 TO IDX-KEY.
           MOVE "WILLIAMS  " TO IDX-NAME.
           MOVE 038750.25 TO IDX-SALARY.
           WRITE IDX-RECORD.
           CLOSE IDX-FILE.
      * TEST-1: Read and verify key
           OPEN INPUT IDX-FILE.
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-KEY = 1001
               DISPLAY "IX104A-TEST-1 PASS"
           ELSE
               DISPLAY "IX104A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Verify name
           IF WS-NAME = "SMITH     "
               DISPLAY "IX104A-TEST-2 PASS"
           ELSE
               DISPLAY "IX104A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Read second record, verify salary
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-SALARY = 052500.50
               DISPLAY "IX104A-TEST-3 PASS"
           ELSE
               DISPLAY "IX104A-TEST-3 FAIL"
           END-IF.
      * TEST-4: Read third record
           READ IDX-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-NAME = "WILLIAMS  "
               DISPLAY "IX104A-TEST-4 PASS"
           ELSE
               DISPLAY "IX104A-TEST-4 FAIL"
           END-IF.
           CLOSE IDX-FILE.
           STOP RUN.
