       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCH-MOVE.
      *> Benchmark: data movement and string operations.
      *> Exercises MOVE, INSPECT, STRING in a tight loop.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER         PIC 9(7) VALUE 0.
       01  WS-LIMIT           PIC 9(7) VALUE 1000000.
       01  WS-SRC             PIC X(30) VALUE "HELLO WORLD BENCHMARK TEST   ".
       01  WS-DEST            PIC X(30) VALUE SPACES.
       01  WS-TALLY           PIC 9(5) VALUE 0.
       01  WS-TOTAL-TALLY     PIC 9(10) VALUE 0.
       PROCEDURE DIVISION.
           PERFORM UNTIL WS-COUNTER >= WS-LIMIT
               MOVE WS-SRC TO WS-DEST
               MOVE 0 TO WS-TALLY
               INSPECT WS-DEST TALLYING WS-TALLY
                   FOR ALL "L"
               ADD WS-TALLY TO WS-TOTAL-TALLY
               ADD 1 TO WS-COUNTER
           END-PERFORM
           DISPLAY "TALLY=" WS-TOTAL-TALLY
           STOP RUN.
