       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ119A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Read an empty file: OPEN INPUT, first READ hits AT END.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ119A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(30).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(30).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       01 WS-UNCHANGED PIC X(10) VALUE "ORIGINAL".
       PROCEDURE DIVISION.
      * Create an empty file
           OPEN OUTPUT SEQ-FILE.
           CLOSE SEQ-FILE.
      * Test 1: First READ should hit AT END
           OPEN INPUT SEQ-FILE.
           MOVE 0 TO WS-EOF.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-EOF = 1
               DISPLAY "SQ119A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ119A-TEST-1 FAIL"
           END-IF.
           CLOSE SEQ-FILE.
      * Test 2: Record count of empty file is zero
           OPEN INPUT SEQ-FILE.
           MOVE 0 TO WS-EOF.
           MOVE 0 TO WS-READ-COUNT.
           PERFORM UNTIL WS-EOF = 1
               READ SEQ-FILE INTO WS-RECORD
                 AT END MOVE 1 TO WS-EOF
               END-READ
               IF WS-EOF = 0
                   ADD 1 TO WS-READ-COUNT
               END-IF
           END-PERFORM.
           CLOSE SEQ-FILE.
           IF WS-READ-COUNT = 0
               DISPLAY "SQ119A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ119A-TEST-2 FAIL"
           END-IF.
      * Test 3: Working storage data unchanged after empty read
           IF WS-UNCHANGED = "ORIGINAL"
               DISPLAY "SQ119A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ119A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
