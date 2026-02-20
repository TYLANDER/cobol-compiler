       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ117A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Overwrite a file by opening OUTPUT again after CLOSE.
      * Verifies the old content is replaced.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ117A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(30).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(30).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       01 WS-FIRST-REC PIC X(30) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write initial records
           OPEN OUTPUT SEQ-FILE.
           MOVE "OLD RECORD ONE" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "OLD RECORD TWO" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "OLD RECORD THREE" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Overwrite with new records
           OPEN OUTPUT SEQ-FILE.
           MOVE "NEW RECORD ALPHA" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "NEW RECORD BETA" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Read back
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
                       MOVE WS-RECORD TO WS-FIRST-REC
                   END-IF
               END-IF
           END-PERFORM.
           CLOSE SEQ-FILE.
      * Test 1: Only 2 records should exist (not 3)
           IF WS-READ-COUNT = 2
               DISPLAY "SQ117A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ117A-TEST-1 FAIL"
           END-IF.
      * Test 2: First record should be the new one
           IF WS-FIRST-REC = "NEW RECORD ALPHA"
               DISPLAY "SQ117A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ117A-TEST-2 FAIL"
           END-IF.
      * Test 3: Old records should not be present
           IF WS-FIRST-REC NOT = "OLD RECORD ONE"
               DISPLAY "SQ117A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ117A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
