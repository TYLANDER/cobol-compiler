       IDENTIFICATION DIVISION.
       PROGRAM-ID. ST103A.
      *
      * NIST CCVS-style test: Multiple ascending SORT keys
      * Records with last-name and first-name fields.
      * SORT ASCENDING last-name then ASCENDING first-name.
      * Verify compound key ordering.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "/tmp/ST103A-IN.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "/tmp/ST103A-OUT.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE ASSIGN TO "/tmp/ST103A-WRK.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 IN-REC PIC X(20).
       FD OUTPUT-FILE.
       01 OUT-REC PIC X(20).
       SD SORT-FILE.
       01 SORT-REC.
          05 SR-LAST-NAME  PIC X(10).
          05 SR-FIRST-NAME PIC X(10).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       01 WS-REC1 PIC X(20) VALUE SPACES.
       01 WS-REC2 PIC X(20) VALUE SPACES.
       01 WS-REC5 PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write 5 records - some share last names
           OPEN OUTPUT INPUT-FILE.
           MOVE "SMITH     ZELDA     " TO IN-REC.
           WRITE IN-REC.
           MOVE "JONES     BOB       " TO IN-REC.
           WRITE IN-REC.
           MOVE "SMITH     ALICE     " TO IN-REC.
           WRITE IN-REC.
           MOVE "ADAMS     CHARLIE   " TO IN-REC.
           WRITE IN-REC.
           MOVE "JONES     ALICE     " TO IN-REC.
           WRITE IN-REC.
           CLOSE INPUT-FILE.
      * Sort ascending by last-name then first-name
           SORT SORT-FILE
             ON ASCENDING KEY SR-LAST-NAME
             ON ASCENDING KEY SR-FIRST-NAME
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
                   IF WS-READ-COUNT = 5
                       MOVE WS-RECORD TO WS-REC5
                   END-IF
               END-IF
           END-PERFORM.
           CLOSE OUTPUT-FILE.
      * Test 1: First record should be ADAMS CHARLIE
      * (ADAMS sorts before JONES and SMITH)
           IF WS-REC1 = "ADAMS     CHARLIE   "
               DISPLAY "ST103A-TEST-1 PASS"
           ELSE
               DISPLAY "ST103A-TEST-1 FAIL"
               DISPLAY "  Expected ADAMS CHARLIE, got " WS-REC1
           END-IF.
      * Test 2: Second record should be JONES ALICE
      * (JONES before SMITH, ALICE before BOB)
           IF WS-REC2 = "JONES     ALICE     "
               DISPLAY "ST103A-TEST-2 PASS"
           ELSE
               DISPLAY "ST103A-TEST-2 FAIL"
               DISPLAY "  Expected JONES ALICE, got " WS-REC2
           END-IF.
      * Test 3: Fifth (last) record should be SMITH ZELDA
           IF WS-REC5 = "SMITH     ZELDA     "
               DISPLAY "ST103A-TEST-3 PASS"
           ELSE
               DISPLAY "ST103A-TEST-3 FAIL"
               DISPLAY "  Expected SMITH ZELDA, got " WS-REC5
           END-IF.
           STOP RUN.
