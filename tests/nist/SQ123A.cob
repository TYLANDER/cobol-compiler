       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ123A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Read all records and verify they are in write order.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ123A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-ORDERED PIC 9 VALUE 1.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       01 WS-PREV PIC X(20) VALUE SPACES.
       01 WS-REC1 PIC X(20) VALUE SPACES.
       01 WS-REC2 PIC X(20) VALUE SPACES.
       01 WS-REC3 PIC X(20) VALUE SPACES.
       01 WS-REC4 PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write 4 records in alphabetical order
           OPEN OUTPUT SEQ-FILE.
           MOVE "AAAA DATA" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "BBBB DATA" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "CCCC DATA" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "DDDD DATA" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Read all records back
           OPEN INPUT SEQ-FILE.
           MOVE 0 TO WS-EOF.
           MOVE 0 TO WS-READ-COUNT.
           READ SEQ-FILE INTO WS-REC1
             AT END MOVE 1 TO WS-EOF
           END-READ.
           READ SEQ-FILE INTO WS-REC2
             AT END MOVE 1 TO WS-EOF
           END-READ.
           READ SEQ-FILE INTO WS-REC3
             AT END MOVE 1 TO WS-EOF
           END-READ.
           READ SEQ-FILE INTO WS-REC4
             AT END MOVE 1 TO WS-EOF
           END-READ.
           CLOSE SEQ-FILE.
      * Test 1: Records in correct order (1 and 2)
           IF WS-REC1 = "AAAA DATA"
             AND WS-REC2 = "BBBB DATA"
               DISPLAY "SQ123A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ123A-TEST-1 FAIL"
           END-IF.
      * Test 2: Records in correct order (3 and 4)
           IF WS-REC3 = "CCCC DATA"
             AND WS-REC4 = "DDDD DATA"
               DISPLAY "SQ123A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ123A-TEST-2 FAIL"
           END-IF.
      * Test 3: Records are in ascending order
           IF WS-REC1 < WS-REC2
             AND WS-REC2 < WS-REC3
             AND WS-REC3 < WS-REC4
               DISPLAY "SQ123A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ123A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
