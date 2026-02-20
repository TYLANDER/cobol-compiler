       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ110A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests sequential file with variable-length records.
      * Uses RECORD CONTAINS 10 TO 80 CHARACTERS to define
      * a variable-length record, writes records of different
      * lengths, reads them back.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ110A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE
           RECORD CONTAINS 10 TO 80 CHARACTERS.
       01 SEQ-RECORD PIC X(80).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(80).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write records of different lengths
           OPEN OUTPUT SEQ-FILE.
           MOVE SPACES TO SEQ-RECORD.
           MOVE "SHORT DATA" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE SPACES TO SEQ-RECORD.
           MOVE "MEDIUM LENGTH RECORD DATA FOR TESTING" TO
               SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE SPACES TO SEQ-RECORD.
           STRING "LONG RECORD WITH EXTENDED CONTENT "
               "THAT FILLS MORE OF THE BUFFER AREA "
               DELIMITED BY SIZE INTO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Test 1: Read back short record
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:10) = "SHORT DATA"
               DISPLAY "SQ110A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ110A-TEST-1 FAIL"
           END-IF.
      * Test 2: Read back medium record
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:38) =
              "MEDIUM LENGTH RECORD DATA FOR TESTING"
               DISPLAY "SQ110A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ110A-TEST-2 FAIL"
           END-IF.
      * Test 3: Read back long record
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-RECORD(1:34) =
              "LONG RECORD WITH EXTENDED CONTENT"
               DISPLAY "SQ110A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ110A-TEST-3 FAIL"
           END-IF.
           CLOSE SEQ-FILE.
           STOP RUN.
