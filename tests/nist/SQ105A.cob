       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ105A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests FILE STATUS checking (00 for success, 10 for EOF).
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ105A.dat"
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-STATUS PIC XX VALUE SPACES.
       01 WS-RECORD PIC X(20).
       01 WS-PASS-COUNT PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: OPEN OUTPUT sets status 00
           OPEN OUTPUT SEQ-FILE.
           IF WS-STATUS = "00"
             ADD 1 TO WS-PASS-COUNT
           END-IF.
      * Test 2: WRITE sets status 00
           MOVE "TEST-RECORD-1" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           IF WS-STATUS = "00"
             ADD 1 TO WS-PASS-COUNT
           END-IF.
      * Test 3: CLOSE sets status 00
           CLOSE SEQ-FILE.
           IF WS-STATUS = "00"
             ADD 1 TO WS-PASS-COUNT
           END-IF.
      * Test 4: READ sets status 00 on success
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-RECORD
             AT END CONTINUE.
           IF WS-STATUS = "00"
             ADD 1 TO WS-PASS-COUNT
           END-IF.
      * Test 5: READ at EOF sets status 10
           READ SEQ-FILE INTO WS-RECORD
             AT END CONTINUE.
           IF WS-STATUS = "10"
             ADD 1 TO WS-PASS-COUNT
           END-IF.
           CLOSE SEQ-FILE.
           IF WS-PASS-COUNT = 5
             DISPLAY "SQ105A-TEST-1 PASS"
           ELSE
             DISPLAY "SQ105A-TEST-1 FAIL"
           END-IF.
           STOP RUN.
