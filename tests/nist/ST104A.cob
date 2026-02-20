       IDENTIFICATION DIVISION.
       PROGRAM-ID. ST104A.
      *
      * NIST CCVS-style test: Mixed ascending/descending SORT keys
      * Records with department (ascending) and salary (descending).
      * Verify highest salary appears first within each department.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "/tmp/ST104A-IN.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "/tmp/ST104A-OUT.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE ASSIGN TO "/tmp/ST104A-WRK.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 IN-REC PIC X(20).
       FD OUTPUT-FILE.
       01 OUT-REC PIC X(20).
       SD SORT-FILE.
       01 SORT-REC.
          05 SR-DEPT   PIC X(05).
          05 SR-SALARY PIC X(05).
          05 SR-NAME   PIC X(10).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-READ-COUNT PIC 9 VALUE 0.
       01 WS-REC1 PIC X(20) VALUE SPACES.
       01 WS-REC2 PIC X(20) VALUE SPACES.
       01 WS-REC3 PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      * Write 4 records: dept + salary + name
           OPEN OUTPUT INPUT-FILE.
           MOVE "SALES50000ALICE     " TO IN-REC.
           WRITE IN-REC.
           MOVE "ENGRN80000BOB       " TO IN-REC.
           WRITE IN-REC.
           MOVE "SALES70000CHARLIE   " TO IN-REC.
           WRITE IN-REC.
           MOVE "ENGRN60000DIANA     " TO IN-REC.
           WRITE IN-REC.
           CLOSE INPUT-FILE.
      * Sort ascending dept, descending salary
           SORT SORT-FILE
             ON ASCENDING KEY SR-DEPT
             ON DESCENDING KEY SR-SALARY
             USING INPUT-FILE
             GIVING OUTPUT-FILE.
      * Expected order:
      *   ENGRN 80000 BOB       (ENGRN first, higher salary)
      *   ENGRN 60000 DIANA     (ENGRN, lower salary)
      *   SALES 70000 CHARLIE   (SALES second, higher salary)
      *   SALES 50000 ALICE     (SALES, lower salary)
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
                   IF WS-READ-COUNT = 3
                       MOVE WS-RECORD TO WS-REC3
                   END-IF
               END-IF
           END-PERFORM.
           CLOSE OUTPUT-FILE.
      * Test 1: First record should be ENGRN 80000 BOB
           IF WS-REC1 = "ENGRN80000BOB       "
               DISPLAY "ST104A-TEST-1 PASS"
           ELSE
               DISPLAY "ST104A-TEST-1 FAIL"
               DISPLAY "  Expected ENGRN80000BOB, got " WS-REC1
           END-IF.
      * Test 2: Second record should be ENGRN 60000 DIANA
           IF WS-REC2 = "ENGRN60000DIANA     "
               DISPLAY "ST104A-TEST-2 PASS"
           ELSE
               DISPLAY "ST104A-TEST-2 FAIL"
               DISPLAY "  Expected ENGRN60000DIANA, got " WS-REC2
           END-IF.
      * Test 3: Third record should be SALES 70000 CHARLIE
           IF WS-REC3 = "SALES70000CHARLIE   "
               DISPLAY "ST104A-TEST-3 PASS"
           ELSE
               DISPLAY "ST104A-TEST-3 FAIL"
               DISPLAY "  Expected SALES70000CHARLIE, got " WS-REC3
           END-IF.
           STOP RUN.
