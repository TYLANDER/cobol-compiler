       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ124A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Large record (PIC X(80)) write and read back.
      * Verifies that 80-byte records survive write/read cycle.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ124A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(80).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(80).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ1 PIC X(80) VALUE SPACES.
       01 WS-READ2 PIC X(80) VALUE SPACES.
       01 WS-READ3 PIC X(80) VALUE SPACES.
       01 WS-EXPECTED1 PIC X(80) VALUE SPACES.
       01 WS-EXPECTED3 PIC X(80) VALUE SPACES.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Prepare large records using INSPECT to fill
           MOVE SPACES TO WS-EXPECTED1.
           INSPECT WS-EXPECTED1
               REPLACING ALL " " BY "A".
           MOVE SPACES TO WS-EXPECTED3.
           INSPECT WS-EXPECTED3
               REPLACING ALL " " BY "Z".
      * Write 3 records: large, short, large
           OPEN OUTPUT SEQ-FILE.
           MOVE WS-EXPECTED1 TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "SHORT IN THE MIDDLE" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE WS-EXPECTED3 TO SEQ-RECORD.
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
                       MOVE WS-RECORD TO WS-READ1
                   END-IF
                   IF WS-READ-COUNT = 2
                       MOVE WS-RECORD TO WS-READ2
                   END-IF
                   IF WS-READ-COUNT = 3
                       MOVE WS-RECORD TO WS-READ3
                   END-IF
               END-IF
           END-PERFORM.
           CLOSE SEQ-FILE.
      * Test 1: 80-char all-A record
           IF WS-READ1 = WS-EXPECTED1
               DISPLAY "SQ124A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ124A-TEST-1 FAIL"
           END-IF.
      * Test 2: Short middle record
           IF WS-READ2 = "SHORT IN THE MIDDLE"
               DISPLAY "SQ124A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ124A-TEST-2 FAIL"
           END-IF.
      * Test 3: 80-char all-Z record
           IF WS-READ3 = WS-EXPECTED3
               DISPLAY "SQ124A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ124A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
