       IDENTIFICATION DIVISION.
       PROGRAM-ID. RL107A.
      *
      * NIST CCVS-style: Relative File — Empty File & AT END
      * Tests edge cases: empty file reads, AT END detection.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REL-FILE ASSIGN TO "/tmp/RL107A.dat"
             ORGANIZATION IS RELATIVE
             ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD REL-FILE.
       01 REL-RECORD PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * TEST-1: Read from empty file triggers AT END
           OPEN OUTPUT REL-FILE.
           CLOSE REL-FILE.
           OPEN INPUT REL-FILE.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           CLOSE REL-FILE.
           IF WS-EOF = 1
               DISPLAY "RL107A-TEST-1 PASS"
           ELSE
               DISPLAY "RL107A-TEST-1 FAIL"
           END-IF.
      * TEST-2: Write 1 record, read past it
           MOVE 0 TO WS-EOF.
           OPEN OUTPUT REL-FILE.
           MOVE "SINGLE RECORD       " TO REL-RECORD.
           WRITE REL-RECORD.
           CLOSE REL-FILE.
           OPEN INPUT REL-FILE.
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-EOF = 0
               DISPLAY "RL107A-TEST-2 PASS"
           ELSE
               DISPLAY "RL107A-TEST-2 FAIL"
           END-IF.
      * TEST-3: Second read triggers AT END
           READ REL-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           CLOSE REL-FILE.
           IF WS-EOF = 1
               DISPLAY "RL107A-TEST-3 PASS"
           ELSE
               DISPLAY "RL107A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
