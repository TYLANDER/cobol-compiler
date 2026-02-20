       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ113A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests multiple sequential files in the same program.
      * Opens two separate files, writes different data to
      * each, closes both, reads both back and verifies.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE-ALPHA ASSIGN TO "/tmp/SQ113A-A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FILE-BETA ASSIGN TO "/tmp/SQ113A-B.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD FILE-ALPHA.
       01 REC-ALPHA PIC X(30).
       FD FILE-BETA.
       01 REC-BETA PIC X(30).
       WORKING-STORAGE SECTION.
       01 WS-REC-A PIC X(30) VALUE SPACES.
       01 WS-REC-B PIC X(30) VALUE SPACES.
       01 WS-EOF-A PIC 9 VALUE 0.
       01 WS-EOF-B PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write to both files
           OPEN OUTPUT FILE-ALPHA.
           OPEN OUTPUT FILE-BETA.
           MOVE "ALPHA FILE RECORD ONE" TO REC-ALPHA.
           WRITE REC-ALPHA.
           MOVE "ALPHA FILE RECORD TWO" TO REC-ALPHA.
           WRITE REC-ALPHA.
           MOVE "BETA FILE RECORD ONE" TO REC-BETA.
           WRITE REC-BETA.
           MOVE "BETA FILE RECORD TWO" TO REC-BETA.
           WRITE REC-BETA.
           CLOSE FILE-ALPHA.
           CLOSE FILE-BETA.
      * Test 1: Read from file ALPHA and verify content
           OPEN INPUT FILE-ALPHA.
           READ FILE-ALPHA INTO WS-REC-A
             AT END MOVE 1 TO WS-EOF-A
           END-READ.
           IF WS-REC-A(1:21) = "ALPHA FILE RECORD ONE"
               DISPLAY "SQ113A-TEST-1 PASS"
           ELSE
               DISPLAY "SQ113A-TEST-1 FAIL"
           END-IF.
      * Test 2: Read from file BETA and verify content
           OPEN INPUT FILE-BETA.
           READ FILE-BETA INTO WS-REC-B
             AT END MOVE 1 TO WS-EOF-B
           END-READ.
           IF WS-REC-B(1:20) = "BETA FILE RECORD ONE"
               DISPLAY "SQ113A-TEST-2 PASS"
           ELSE
               DISPLAY "SQ113A-TEST-2 FAIL"
           END-IF.
      * Test 3: Read second record from each simultaneously
           READ FILE-ALPHA INTO WS-REC-A
             AT END MOVE 1 TO WS-EOF-A
           END-READ.
           READ FILE-BETA INTO WS-REC-B
             AT END MOVE 1 TO WS-EOF-B
           END-READ.
           IF WS-REC-A(1:21) = "ALPHA FILE RECORD TWO"
           AND WS-REC-B(1:20) = "BETA FILE RECORD TWO"
               DISPLAY "SQ113A-TEST-3 PASS"
           ELSE
               DISPLAY "SQ113A-TEST-3 FAIL"
           END-IF.
           CLOSE FILE-ALPHA.
           CLOSE FILE-BETA.
           STOP RUN.
