       IDENTIFICATION DIVISION.
       PROGRAM-ID. ST101A.
      *
      * NIST CCVS-style test: Basic ascending SORT
      * Write 5 records with alphabetic keys in random order,
      * SORT ASCENDING, verify correct order.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "/tmp/ST101A-IN.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "/tmp/ST101A-OUT.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE ASSIGN TO "/tmp/ST101A-WRK.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 IN-REC PIC X(20).
       FD OUTPUT-FILE.
       01 OUT-REC PIC X(20).
       SD SORT-FILE.
       01 SORT-REC.
          05 SORT-KEY PIC X(10).
          05 SORT-DATA PIC X(10).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       01 WS-REC1 PIC X(20) VALUE SPACES.
       01 WS-REC3 PIC X(20) VALUE SPACES.
       01 WS-REC5 PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write 5 unsorted records
           OPEN OUTPUT INPUT-FILE.
           MOVE "ECHO      DATA-E    " TO IN-REC.
           WRITE IN-REC.
           MOVE "CHARLIE   DATA-C    " TO IN-REC.
           WRITE IN-REC.
           MOVE "ALPHA     DATA-A    " TO IN-REC.
           WRITE IN-REC.
           MOVE "DELTA     DATA-D    " TO IN-REC.
           WRITE IN-REC.
           MOVE "BRAVO     DATA-B    " TO IN-REC.
           WRITE IN-REC.
           CLOSE INPUT-FILE.
      * Sort ascending by key
           SORT SORT-FILE
             ON ASCENDING KEY SORT-KEY
             USING INPUT-FILE
             GIVING OUTPUT-FILE.
      * Read sorted output and capture records 1, 3, 5
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
      * Test 1: First record should be ALPHA
           IF WS-REC1(1:10) = "ALPHA     "
               DISPLAY "ST101A-TEST-1 PASS"
           ELSE
               DISPLAY "ST101A-TEST-1 FAIL"
               DISPLAY "  Expected ALPHA, got " WS-REC1(1:10)
           END-IF.
      * Test 2: Third record should be CHARLIE
           IF WS-REC3(1:10) = "CHARLIE   "
               DISPLAY "ST101A-TEST-2 PASS"
           ELSE
               DISPLAY "ST101A-TEST-2 FAIL"
               DISPLAY "  Expected CHARLIE, got " WS-REC3(1:10)
           END-IF.
      * Test 3: Total record count should be 5
           IF WS-READ-COUNT = 5
               DISPLAY "ST101A-TEST-3 PASS"
           ELSE
               DISPLAY "ST101A-TEST-3 FAIL"
               DISPLAY "  Expected 5 records, got " WS-READ-COUNT
           END-IF.
           STOP RUN.
