       IDENTIFICATION DIVISION.
       PROGRAM-ID. ST102A.
      *
      * NIST CCVS-style test: Descending SORT
      * Write 4 records with numeric keys, SORT DESCENDING,
      * verify order is highest to lowest.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "/tmp/ST102A-IN.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "/tmp/ST102A-OUT.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE ASSIGN TO "/tmp/ST102A-WRK.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 IN-REC PIC X(20).
       FD OUTPUT-FILE.
       01 OUT-REC PIC X(20).
       SD SORT-FILE.
       01 SORT-REC.
          05 SORT-KEY PIC X(02).
          05 SORT-DATA PIC X(18).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       01 WS-REC1 PIC X(20) VALUE SPACES.
       01 WS-REC2 PIC X(20) VALUE SPACES.
       01 WS-REC4 PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write 4 unsorted records with numeric keys
           OPEN OUTPUT INPUT-FILE.
           MOVE "01RECORD-ONE        " TO IN-REC.
           WRITE IN-REC.
           MOVE "04RECORD-FOUR       " TO IN-REC.
           WRITE IN-REC.
           MOVE "02RECORD-TWO        " TO IN-REC.
           WRITE IN-REC.
           MOVE "03RECORD-THREE      " TO IN-REC.
           WRITE IN-REC.
           CLOSE INPUT-FILE.
      * Sort descending by key
           SORT SORT-FILE
             ON DESCENDING KEY SORT-KEY
             USING INPUT-FILE
             GIVING OUTPUT-FILE.
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
                   IF WS-READ-COUNT = 2
                       MOVE WS-RECORD TO WS-REC2
                   END-IF
                   IF WS-READ-COUNT = 4
                       MOVE WS-RECORD TO WS-REC4
                   END-IF
               END-IF
           END-PERFORM.
           CLOSE OUTPUT-FILE.
      * Test 1: First record should be 04 (highest)
           IF WS-REC1(1:2) = "04"
               DISPLAY "ST102A-TEST-1 PASS"
           ELSE
               DISPLAY "ST102A-TEST-1 FAIL"
               DISPLAY "  Expected 04, got " WS-REC1(1:2)
           END-IF.
      * Test 2: Second record should be 03
           IF WS-REC2(1:2) = "03"
               DISPLAY "ST102A-TEST-2 PASS"
           ELSE
               DISPLAY "ST102A-TEST-2 FAIL"
               DISPLAY "  Expected 03, got " WS-REC2(1:2)
           END-IF.
      * Test 3: Last record should be 01 (lowest)
           IF WS-REC4(1:2) = "01"
               DISPLAY "ST102A-TEST-3 PASS"
           ELSE
               DISPLAY "ST102A-TEST-3 FAIL"
               DISPLAY "  Expected 01, got " WS-REC4(1:2)
           END-IF.
           STOP RUN.
