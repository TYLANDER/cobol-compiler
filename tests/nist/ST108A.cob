       IDENTIFICATION DIVISION.
       PROGRAM-ID. ST108A.
      *
      * NIST CCVS-style test: Sort preserving data fields
      * Verify non-key data fields survive the sort unchanged.
      * Write records where data portion has distinct values,
      * sort by key, verify both order AND data integrity.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "/tmp/ST108A-IN.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "/tmp/ST108A-OUT.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE ASSIGN TO "/tmp/ST108A-WRK.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 IN-REC PIC X(30).
       FD OUTPUT-FILE.
       01 OUT-REC PIC X(30).
       SD SORT-FILE.
       01 SORT-REC.
          05 SORT-KEY  PIC X(05).
          05 SORT-DATA PIC X(25).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(30).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       01 WS-REC1 PIC X(30) VALUE SPACES.
       01 WS-REC2 PIC X(30) VALUE SPACES.
       01 WS-REC4 PIC X(30) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write 4 records with distinct key + data combinations
           OPEN OUTPUT INPUT-FILE.
           MOVE "DDDDDPAYLOAD-FOR-D-RECORD    " TO IN-REC.
           WRITE IN-REC.
           MOVE "AAAAAPAYLOAD-FOR-A-RECORD    " TO IN-REC.
           WRITE IN-REC.
           MOVE "CCCCCPAYLOAD-FOR-C-RECORD    " TO IN-REC.
           WRITE IN-REC.
           MOVE "BBBBBPAYLOAD-FOR-B-RECORD    " TO IN-REC.
           WRITE IN-REC.
           CLOSE INPUT-FILE.
      * Sort ascending by key
           SORT SORT-FILE
             ON ASCENDING KEY SORT-KEY
             USING INPUT-FILE
             GIVING OUTPUT-FILE.
      * Expected order: AAAAA, BBBBB, CCCCC, DDDDD
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
      * Test 1: First record key is AAAAA with correct data
           IF WS-REC1 = "AAAAAPAYLOAD-FOR-A-RECORD    "
               DISPLAY "ST108A-TEST-1 PASS"
           ELSE
               DISPLAY "ST108A-TEST-1 FAIL"
               DISPLAY "  Expected AAAAA+A-payload, got " WS-REC1
           END-IF.
      * Test 2: Second record key is BBBBB with correct data
           IF WS-REC2 = "BBBBBPAYLOAD-FOR-B-RECORD    "
               DISPLAY "ST108A-TEST-2 PASS"
           ELSE
               DISPLAY "ST108A-TEST-2 FAIL"
               DISPLAY "  Expected BBBBB+B-payload, got " WS-REC2
           END-IF.
      * Test 3: Fourth record key is DDDDD with correct data
           IF WS-REC4 = "DDDDDPAYLOAD-FOR-D-RECORD    "
               DISPLAY "ST108A-TEST-3 PASS"
           ELSE
               DISPLAY "ST108A-TEST-3 FAIL"
               DISPLAY "  Expected DDDDD+D-payload, got " WS-REC4
           END-IF.
           STOP RUN.
