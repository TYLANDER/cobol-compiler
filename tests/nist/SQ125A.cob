       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ125A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Write numeric data, read back and verify arithmetic.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ125A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-NUM-OUT PIC 9(5).
       01 WS-NUM-READ PIC 9(5).
       01 WS-SUM PIC 9(6) VALUE 0.
       01 WS-FIRST-NUM PIC 9(5) VALUE 0.
       01 WS-SECOND-NUM PIC 9(5) VALUE 0.
       01 WS-THIRD-NUM PIC 9(5) VALUE 0.
       01 WS-PRODUCT PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
      * Write 3 records with numeric content
           OPEN OUTPUT SEQ-FILE.
           MOVE 00100 TO WS-NUM-OUT.
           MOVE WS-NUM-OUT TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE 00250 TO WS-NUM-OUT.
           MOVE WS-NUM-OUT TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE 00650 TO WS-NUM-OUT.
           MOVE WS-NUM-OUT TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Read back numeric data
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           MOVE WS-RECORD TO WS-NUM-READ.
           MOVE WS-NUM-READ TO WS-FIRST-NUM.
           ADD WS-NUM-READ TO WS-SUM.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           MOVE WS-RECORD TO WS-NUM-READ.
           MOVE WS-NUM-READ TO WS-SECOND-NUM.
           ADD WS-NUM-READ TO WS-SUM.
           READ SEQ-FILE INTO WS-RECORD
             AT END MOVE 1 TO WS-EOF
           END-READ.
           MOVE WS-RECORD TO WS-NUM-READ.
           MOVE WS-NUM-READ TO WS-THIRD-NUM.
           ADD WS-NUM-READ TO WS-SUM.
           CLOSE SEQ-FILE.
      * Test 1: First value read back is 100
           IF WS-FIRST-NUM = 00100
               DISPLAY "SQ125A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ125A-TEST-1 FAIL"
           END-IF.
      * Test 2: Sum of all three = 1000
           IF WS-SUM = 001000
               DISPLAY "SQ125A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ125A-TEST-2 FAIL"
           END-IF.
      * Test 3: Third value minus first = 550
           SUBTRACT WS-FIRST-NUM FROM WS-THIRD-NUM
             GIVING WS-PRODUCT.
           IF WS-PRODUCT = 000550
               DISPLAY "SQ125A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ125A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
