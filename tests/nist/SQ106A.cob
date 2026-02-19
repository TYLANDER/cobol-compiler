       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ106A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests multiple sequential files open simultaneously.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE-A ASSIGN TO "/tmp/SQ106A-A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FILE-B ASSIGN TO "/tmp/SQ106A-B.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD FILE-A.
       01 REC-A PIC X(20).
       FD FILE-B.
       01 REC-B PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-REC-A PIC X(20).
       01 WS-REC-B PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Write to both files simultaneously
           OPEN OUTPUT FILE-A.
           OPEN OUTPUT FILE-B.
           MOVE "DATA FOR FILE A" TO REC-A.
           WRITE REC-A.
           MOVE "DATA FOR FILE B" TO REC-B.
           WRITE REC-B.
           CLOSE FILE-A.
           CLOSE FILE-B.
      * Read back from file A
           OPEN INPUT FILE-A.
           READ FILE-A INTO WS-REC-A
             AT END MOVE 1 TO WS-EOF
           END-READ.
           CLOSE FILE-A.
           IF WS-REC-A(1:15) = "DATA FOR FILE A"
               DISPLAY "SQ106A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ106A-TEST-1 FAIL"
           END-IF.
      * Test 2: Read back from file B
           MOVE 0 TO WS-EOF.
           OPEN INPUT FILE-B.
           READ FILE-B INTO WS-REC-B
             AT END MOVE 1 TO WS-EOF
           END-READ.
           CLOSE FILE-B.
           IF WS-REC-B(1:15) = "DATA FOR FILE B"
               DISPLAY "SQ106A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ106A-TEST-2 FAIL"
           END-IF.
      * Test 3: Both files open for input at same time
           MOVE 0 TO WS-EOF.
           MOVE SPACES TO WS-REC-A.
           MOVE SPACES TO WS-REC-B.
           OPEN INPUT FILE-A.
           OPEN INPUT FILE-B.
           READ FILE-A INTO WS-REC-A
             AT END MOVE 1 TO WS-EOF
           END-READ.
           READ FILE-B INTO WS-REC-B
             AT END MOVE 1 TO WS-EOF
           END-READ.
           CLOSE FILE-A.
           CLOSE FILE-B.
           IF WS-REC-A(1:15) = "DATA FOR FILE A"
           AND WS-REC-B(1:15) = "DATA FOR FILE B"
               DISPLAY "SQ106A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ106A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
