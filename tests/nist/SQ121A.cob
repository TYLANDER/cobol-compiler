       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ121A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Multiple WRITE statements in a single OPEN/CLOSE cycle.
      * Verifies each record is preserved in order.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ121A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       01 WS-REC1 PIC X(20) VALUE SPACES.
       01 WS-REC4 PIC X(20) VALUE SPACES.
       01 WS-REC7 PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write 7 records in one OPEN/CLOSE
           OPEN OUTPUT SEQ-FILE.
           MOVE "REC-WRITE-01" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "REC-WRITE-02" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "REC-WRITE-03" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "REC-WRITE-04" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "REC-WRITE-05" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "REC-WRITE-06" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "REC-WRITE-07" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Read all and capture records 1, 4, and 7
           OPEN INPUT SEQ-FILE.
           MOVE 0 TO WS-EOF.
           MOVE 0 TO WS-READ-COUNT.
           PERFORM UNTIL WS-EOF = 1
               READ SEQ-FILE INTO WS-RECORD
                 AT END MOVE 1 TO WS-EOF
               END-READ
               IF WS-EOF = 0
                   ADD 1 TO WS-READ-COUNT
                   IF WS-READ-COUNT = 1
                       MOVE WS-RECORD TO WS-REC1
                   END-IF
                   IF WS-READ-COUNT = 4
                       MOVE WS-RECORD TO WS-REC4
                   END-IF
                   IF WS-READ-COUNT = 7
                       MOVE WS-RECORD TO WS-REC7
                   END-IF
               END-IF
           END-PERFORM.
           CLOSE SEQ-FILE.
      * Test 1: First record correct
           IF WS-REC1 = "REC-WRITE-01"
               DISPLAY "SQ121A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ121A-TEST-1 FAIL"
           END-IF.
      * Test 2: Fourth record correct
           IF WS-REC4 = "REC-WRITE-04"
               DISPLAY "SQ121A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ121A-TEST-2 FAIL"
           END-IF.
      * Test 3: Seventh (last) record correct
           IF WS-REC7 = "REC-WRITE-07"
               DISPLAY "SQ121A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ121A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
