       IDENTIFICATION DIVISION.
       PROGRAM-ID. ST107A.
      *
      * NIST CCVS-style test: Large record SORT
      * Records with PIC X(80) size, key at beginning.
      * Write 3 records, sort, verify order.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "/tmp/ST107A-IN.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "/tmp/ST107A-OUT.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE ASSIGN TO "/tmp/ST107A-WRK.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 IN-REC PIC X(80).
       FD OUTPUT-FILE.
       01 OUT-REC PIC X(80).
       SD SORT-FILE.
       01 SORT-REC.
          05 SORT-KEY  PIC X(10).
          05 SORT-DATA PIC X(70).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(80).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       01 WS-REC1 PIC X(80) VALUE SPACES.
       01 WS-REC3 PIC X(80) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write 3 large records in reverse order
           OPEN OUTPUT INPUT-FILE.
           MOVE SPACES TO IN-REC.
           MOVE "ZEBRA     " TO IN-REC(1:10).
           MOVE "PAYLOAD-FOR-ZEBRA-RECORD-WITH-EXTRA-DATA"
               TO IN-REC(11:40).
           WRITE IN-REC.
           MOVE SPACES TO IN-REC.
           MOVE "MIDDLE    " TO IN-REC(1:10).
           MOVE "PAYLOAD-FOR-MIDDLE-RECORD-CONTENT-HERE"
               TO IN-REC(11:38).
           WRITE IN-REC.
           MOVE SPACES TO IN-REC.
           MOVE "ALPHA     " TO IN-REC(1:10).
           MOVE "PAYLOAD-FOR-ALPHA-RECORD-FIRST-IN-ORDER"
               TO IN-REC(11:39).
           WRITE IN-REC.
           CLOSE INPUT-FILE.
      * Sort ascending by key
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
                   IF WS-READ-COUNT = 1
                       MOVE WS-RECORD TO WS-REC1
                   END-IF
                   IF WS-READ-COUNT = 3
                       MOVE WS-RECORD TO WS-REC3
                   END-IF
               END-IF
           END-PERFORM.
           CLOSE OUTPUT-FILE.
      * Test 1: First record key should be ALPHA
           IF WS-REC1(1:10) = "ALPHA     "
               DISPLAY "ST107A-TEST-1 PASS"
           ELSE
               DISPLAY "ST107A-TEST-1 FAIL"
               DISPLAY "  Expected ALPHA, got " WS-REC1(1:10)
           END-IF.
      * Test 2: Third (last) record key should be ZEBRA
           IF WS-REC3(1:10) = "ZEBRA     "
               DISPLAY "ST107A-TEST-2 PASS"
           ELSE
               DISPLAY "ST107A-TEST-2 FAIL"
               DISPLAY "  Expected ZEBRA, got " WS-REC3(1:10)
           END-IF.
           STOP RUN.
