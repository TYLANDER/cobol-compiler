       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ115A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests large record count: writes 100 numbered records,
      * reads them all back, counts them, verifies total = 100.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ115A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(30).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(30) VALUE SPACES.
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-WRITE-IX PIC 9(3) VALUE 0.
       01 WS-READ-COUNT PIC 9(3) VALUE 0.
       01 WS-NUM-STR PIC 9(3).
       01 WS-FIRST-REC PIC X(30) VALUE SPACES.
       01 WS-LAST-REC  PIC X(30) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write 100 numbered records
           OPEN OUTPUT SEQ-FILE.
           PERFORM VARYING WS-WRITE-IX FROM 1 BY 1
             UNTIL WS-WRITE-IX > 100
               MOVE SPACES TO SEQ-RECORD
               MOVE WS-WRITE-IX TO WS-NUM-STR
               STRING "RECORD-" WS-NUM-STR
                 DELIMITED BY SIZE INTO SEQ-RECORD
               WRITE SEQ-RECORD
           END-PERFORM.
           CLOSE SEQ-FILE.
      * Read all records and count them
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
      * Test 1: Verify first record started with RECORD-001
           IF WS-FIRST-REC = "RECORD-001"
               DISPLAY "SQ115A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ115A-TEST-1 FAIL"
           END-IF.
      * Test 2: Verify last record was RECORD-100
           IF WS-LAST-REC = "RECORD-100"
               DISPLAY "SQ115A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ115A-TEST-2 FAIL"
           END-IF.
      * Test 3: Verify total count = 100
           IF WS-READ-COUNT = 100
               DISPLAY "SQ115A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ115A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
