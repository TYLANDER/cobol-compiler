       IDENTIFICATION DIVISION.
       PROGRAM-ID. ST105A.
      *
      * NIST CCVS-style test: Numeric sort keys
      * Records with PIC 9(5) numeric keys in random order.
      * SORT ASCENDING, verify correct numeric ordering.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "/tmp/ST105A-IN.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "/tmp/ST105A-OUT.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE ASSIGN TO "/tmp/ST105A-WRK.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 IN-REC PIC X(15).
       FD OUTPUT-FILE.
       01 OUT-REC PIC X(15).
       SD SORT-FILE.
       01 SORT-REC.
          05 SORT-KEY  PIC X(05).
          05 SORT-DATA PIC X(10).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(15).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       01 WS-REC1 PIC X(15) VALUE SPACES.
       01 WS-REC3 PIC X(15) VALUE SPACES.
       01 WS-REC5 PIC X(15) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write 5 records with numeric keys
           OPEN OUTPUT INPUT-FILE.
           MOVE "00100DATA-100  " TO IN-REC.
           WRITE IN-REC.
           MOVE "00005DATA-005  " TO IN-REC.
           WRITE IN-REC.
           MOVE "00050DATA-050  " TO IN-REC.
           WRITE IN-REC.
           MOVE "00001DATA-001  " TO IN-REC.
           WRITE IN-REC.
           MOVE "00500DATA-500  " TO IN-REC.
           WRITE IN-REC.
           CLOSE INPUT-FILE.
      * Sort ascending by numeric key
           SORT SORT-FILE
             ON ASCENDING KEY SORT-KEY
             USING INPUT-FILE
             GIVING OUTPUT-FILE.
      * Expected order: 00001, 00005, 00050, 00100, 00500
      * Read sorted output
           OPEN INPUT OUTPUT-FILE.
           MOVE 0 TO WS-EOF.
           MOVE 0 TO WS-READ-COUNT.
           PERFORM UNTIL WS-EOF = 1
               READ OUTPUT-FILE INTO WS-RECORD
                 AT END MOVE 1 TO WS-EOF
               END-READ
               IF WS-EOF = 0
                   ADD 1 TO WS-READ-COUNT
                   IF WS-READ-COUNT = 1
                       MOVE WS-RECORD TO WS-REC1
                   END-IF
                   IF WS-READ-COUNT = 3
                       MOVE WS-RECORD TO WS-REC3
                   END-IF
                   IF WS-READ-COUNT = 5
                       MOVE WS-RECORD TO WS-REC5
                   END-IF
               END-IF
           END-PERFORM.
           CLOSE OUTPUT-FILE.
      * Test 1: First record should be 00001
           IF WS-REC1(1:5) = "00001"
               DISPLAY "ST105A-TEST-1 PASS"
           ELSE
               DISPLAY "ST105A-TEST-1 FAIL"
               DISPLAY "  Expected 00001, got " WS-REC1(1:5)
           END-IF.
      * Test 2: Third record should be 00050
           IF WS-REC3(1:5) = "00050"
               DISPLAY "ST105A-TEST-2 PASS"
           ELSE
               DISPLAY "ST105A-TEST-2 FAIL"
               DISPLAY "  Expected 00050, got " WS-REC3(1:5)
           END-IF.
      * Test 3: Fifth (last) record should be 00500
           IF WS-REC5(1:5) = "00500"
               DISPLAY "ST105A-TEST-3 PASS"
           ELSE
               DISPLAY "ST105A-TEST-3 FAIL"
               DISPLAY "  Expected 00500, got " WS-REC5(1:5)
           END-IF.
           STOP RUN.
