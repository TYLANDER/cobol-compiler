       IDENTIFICATION DIVISION.
       PROGRAM-ID. ST106A.
      *
      * NIST CCVS-style test: Single record SORT (edge case)
      * Write 1 record, sort, read back, verify unchanged.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "/tmp/ST106A-IN.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "/tmp/ST106A-OUT.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE ASSIGN TO "/tmp/ST106A-WRK.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 IN-REC PIC X(20).
       FD OUTPUT-FILE.
       01 OUT-REC PIC X(20).
       SD SORT-FILE.
       01 SORT-REC.
          05 SORT-KEY  PIC X(10).
          05 SORT-DATA PIC X(10).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write a single record
           OPEN OUTPUT INPUT-FILE.
           MOVE "ONLY-ONE  DATA-ONLY " TO IN-REC.
           WRITE IN-REC.
           CLOSE INPUT-FILE.
      * Sort ascending by key (trivial with 1 record)
           SORT SORT-FILE
             ON ASCENDING KEY SORT-KEY
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
               END-IF
           END-PERFORM.
           CLOSE OUTPUT-FILE.
      * Test 1: Should have exactly 1 record
           IF WS-READ-COUNT = 1
               DISPLAY "ST106A-TEST-1 PASS"
           ELSE
               DISPLAY "ST106A-TEST-1 FAIL"
               DISPLAY "  Expected 1 record, got " WS-READ-COUNT
           END-IF.
      * Test 2: Record content should be preserved
           IF WS-RECORD = "ONLY-ONE  DATA-ONLY "
               DISPLAY "ST106A-TEST-2 PASS"
           ELSE
               DISPLAY "ST106A-TEST-2 FAIL"
               DISPLAY "  Expected ONLY-ONE DATA-ONLY, got "
                   WS-RECORD
           END-IF.
           STOP RUN.
