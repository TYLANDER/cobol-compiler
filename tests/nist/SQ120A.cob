       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ120A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Append-like test: write records, close, rewrite all.
      * Since sequential files don't support APPEND, opening
      * OUTPUT again replaces the file. Verify the new content.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ120A.dat"
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
       01 WS-LAST-REC PIC X(30) VALUE SPACES.
       PROCEDURE DIVISION.
      * Phase 1: Write initial 2 records
           OPEN OUTPUT SEQ-FILE.
           MOVE "INITIAL REC A" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "INITIAL REC B" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Phase 2: Rewrite file with 3 different records
           OPEN OUTPUT SEQ-FILE.
           MOVE "REPLACED REC X" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "REPLACED REC Y" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "REPLACED REC Z" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Read back all records
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
                   MOVE WS-RECORD TO WS-LAST-REC
               END-IF
           END-PERFORM.
           CLOSE SEQ-FILE.
      * Test 1: File now has 3 records (not 2)
           IF WS-READ-COUNT = 3
               DISPLAY "SQ120A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ120A-TEST-1 FAIL"
           END-IF.
      * Test 2: First record is from phase 2
           IF WS-FIRST-REC = "REPLACED REC X"
               DISPLAY "SQ120A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ120A-TEST-2 FAIL"
           END-IF.
      * Test 3: Last record is from phase 2
           IF WS-LAST-REC = "REPLACED REC Z"
               DISPLAY "SQ120A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ120A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
