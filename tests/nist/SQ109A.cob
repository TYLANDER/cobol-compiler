       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ109A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests WRITE with ADVANCING (BEFORE/AFTER ADVANCING).
      * Writes records with advancing clauses, then reads
      * them back to verify content is intact.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ109A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(30).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(30).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write records with ADVANCING clauses
           OPEN OUTPUT SEQ-FILE.
           MOVE "BEFORE-ADV RECORD ONE" TO SEQ-RECORD.
           WRITE SEQ-RECORD BEFORE ADVANCING 1 LINE.
           MOVE "AFTER-ADV RECORD TWO" TO SEQ-RECORD.
           WRITE SEQ-RECORD AFTER ADVANCING 2 LINES.
           MOVE "BEFORE-ADV RECORD THREE" TO SEQ-RECORD.
           WRITE SEQ-RECORD BEFORE ADVANCING 1 LINE.
           CLOSE SEQ-FILE.
      * Test 1: Read first record back
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:21) = "BEFORE-ADV RECORD ONE"
               DISPLAY "SQ109A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ109A-TEST-1 FAIL"
           END-IF.
      * Test 2: Read second record (written with AFTER ADV)
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:20) = "AFTER-ADV RECORD TWO"
               DISPLAY "SQ109A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ109A-TEST-2 FAIL"
           END-IF.
      * Test 3: Read third record and verify no premature EOF
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-EOF = 0
           AND WS-RECORD(1:23) =
               "BEFORE-ADV RECORD THREE"
               DISPLAY "SQ109A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ109A-TEST-3 FAIL"
           END-IF.
           CLOSE SEQ-FILE.
           STOP RUN.
