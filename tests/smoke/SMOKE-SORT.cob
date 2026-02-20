       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-SORT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "SORT-IN.DAT"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "SORT-OUT.DAT"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE ASSIGN TO "SORT-WRK.DAT"
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
       01 WS-REC PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Write unsorted data to input file
           OPEN OUTPUT INPUT-FILE.
           MOVE "CHERRY    FRUIT3    " TO IN-REC.
           WRITE IN-REC.
           MOVE "APPLE     FRUIT1    " TO IN-REC.
           WRITE IN-REC.
           MOVE "BANANA    FRUIT2    " TO IN-REC.
           WRITE IN-REC.
           CLOSE INPUT-FILE.

      * Sort the file
           SORT SORT-FILE
             ON ASCENDING KEY SORT-KEY
             USING INPUT-FILE
             GIVING OUTPUT-FILE.

      * Read sorted output and verify
           OPEN INPUT OUTPUT-FILE.
           READ OUTPUT-FILE INTO WS-REC
             AT END MOVE 1 TO WS-EOF
           END-READ.
           IF WS-REC(1:5) = "APPLE"
               DISPLAY "PASS - FIRST RECORD CORRECT"
           ELSE
               DISPLAY "FAIL - EXPECTED APPLE GOT " WS-REC
           END-IF.
           CLOSE OUTPUT-FILE.
           STOP RUN.
