       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ111A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests OPEN EXTEND to append to an existing file.
      * Writes initial records, closes, reopens with EXTEND,
      * writes more, then reads all back to verify.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ111A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(30).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(30).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write initial records
           OPEN OUTPUT SEQ-FILE.
           MOVE "INITIAL RECORD ONE" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "INITIAL RECORD TWO" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Append additional records using EXTEND
           OPEN EXTEND SEQ-FILE.
           MOVE "APPENDED RECORD THREE" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "APPENDED RECORD FOUR" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Test 1: Read first record (from initial write)
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:18) = "INITIAL RECORD ONE"
               DISPLAY "SQ111A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ111A-TEST-1 FAIL"
           END-IF.
      * Test 2: Read second record and skip to third
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:21) = "APPENDED RECORD THREE"
               DISPLAY "SQ111A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ111A-TEST-2 FAIL"
           END-IF.
      * Test 3: Verify all 4 records present (read 4th, then EOF)
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:20) = "APPENDED RECORD FOUR"
               READ SEQ-FILE INTO WS-RECORD
                 AT END MOVE 1 TO WS-EOF
               END-READ
               IF WS-EOF = 1
                   DISPLAY "SQ111A-TEST-3 PASS"
               ELSE
                   DISPLAY "SQ111A-TEST-3 FAIL"
               END-IF
           ELSE
               DISPLAY "SQ111A-TEST-3 FAIL"
           END-IF.
           CLOSE SEQ-FILE.
           STOP RUN.
