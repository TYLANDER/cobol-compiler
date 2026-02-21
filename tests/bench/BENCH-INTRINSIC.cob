       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCH-INTRINSIC.
      *> Benchmark: intrinsic function calls.
      *> Exercises LENGTH, REVERSE, UPPER-CASE, MOD, MAX, MIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER         PIC 9(7) VALUE 0.
       01  WS-LIMIT           PIC 9(7) VALUE 2000000.
       01  WS-STR             PIC X(20) VALUE "hello world test    ".
       01  WS-LEN             PIC 9(5) VALUE 0.
       01  WS-ACCUM           PIC 9(10) VALUE 0.
       01  WS-A               PIC 9(5) VALUE 0.
       01  WS-B               PIC 9(5) VALUE 0.
       01  WS-MAX-VAL         PIC 9(5) VALUE 0.
       PROCEDURE DIVISION.
           PERFORM UNTIL WS-COUNTER >= WS-LIMIT
               COMPUTE WS-LEN = FUNCTION LENGTH(WS-STR)
               ADD WS-LEN TO WS-ACCUM
               COMPUTE WS-A = FUNCTION MOD(WS-COUNTER, 100)
               COMPUTE WS-B = FUNCTION MOD(WS-COUNTER, 73)
               COMPUTE WS-MAX-VAL = FUNCTION MAX(WS-A, WS-B)
               ADD WS-MAX-VAL TO WS-ACCUM
               ADD 1 TO WS-COUNTER
           END-PERFORM
           DISPLAY "ACCUM=" WS-ACCUM
           STOP RUN.
