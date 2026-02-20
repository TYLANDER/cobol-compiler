       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ112A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests READ INTO to read directly into a working
      * storage variable.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ112A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(30).
       WORKING-STORAGE SECTION.
       01 WS-TARGET-1 PIC X(30) VALUE SPACES.
       01 WS-TARGET-2 PIC X(30) VALUE SPACES.
       01 WS-TARGET-3 PIC X(30) VALUE SPACES.
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write records to the file
           OPEN OUTPUT SEQ-FILE.
           MOVE "READ-INTO RECORD ALPHA" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "READ-INTO RECORD BETA" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           MOVE "READ-INTO RECORD GAMMA" TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.
      * Test 1: READ INTO first working storage target
           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE INTO WS-TARGET-1
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-TARGET-1(1:22) = "READ-INTO RECORD ALPHA"
               DISPLAY "SQ112A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ112A-TEST-1 FAIL"
           END-IF.
      * Test 2: READ INTO second target, verify first unchanged
           READ SEQ-FILE INTO WS-TARGET-2
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-TARGET-2(1:21) = "READ-INTO RECORD BETA"
           AND WS-TARGET-1(1:22) = "READ-INTO RECORD ALPHA"
               DISPLAY "SQ112A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ112A-TEST-2 FAIL"
           END-IF.
      * Test 3: READ INTO third target, verify all three
           READ SEQ-FILE INTO WS-TARGET-3
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-TARGET-3(1:22) = "READ-INTO RECORD GAMMA"
           AND WS-TARGET-1(1:22) = "READ-INTO RECORD ALPHA"
           AND WS-TARGET-2(1:21) = "READ-INTO RECORD BETA"
               DISPLAY "SQ112A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ112A-TEST-3 FAIL"
           END-IF.
           CLOSE SEQ-FILE.
           STOP RUN.
