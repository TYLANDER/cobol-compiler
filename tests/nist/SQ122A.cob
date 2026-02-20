       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ122A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Write multiple records then read back in order.
      * Tests that records written in sequence can be read
      * back in the same order with correct content.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ122A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(30).
       WORKING-STORAGE SECTION.
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ1 PIC X(30) VALUE SPACES.
       01 WS-READ2 PIC X(30) VALUE SPACES.
       01 WS-READ3 PIC X(30) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write 3 records directly
           OPEN OUTPUT SEQ-FILE.
           MOVE "WS SOURCE RECORD ONE" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "WS SOURCE RECORD TWO" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "WS SOURCE RECORD THREE" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Read back
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-READ1
             AT END MOVE 1 TO WS-EOF
           END-READ.
           READ SEQ-FILE INTO WS-READ2
             AT END MOVE 1 TO WS-EOF
           END-READ.
           READ SEQ-FILE INTO WS-READ3
             AT END MOVE 1 TO WS-EOF
           END-READ.
           CLOSE SEQ-FILE.
      * Test 1: First record
           IF WS-READ1 = "WS SOURCE RECORD ONE"
               DISPLAY "SQ122A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ122A-TEST-1 FAIL"
           END-IF.
      * Test 2: Second record
           IF WS-READ2 = "WS SOURCE RECORD TWO"
               DISPLAY "SQ122A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ122A-TEST-2 FAIL"
           END-IF.
      * Test 3: Third record
           IF WS-READ3 = "WS SOURCE RECORD THREE"
               DISPLAY "SQ122A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ122A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
