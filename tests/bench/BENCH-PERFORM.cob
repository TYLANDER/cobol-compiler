       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCH-PERFORM.
      *> Benchmark: PERFORM loop overhead.
      *> Tests nested PERFORM with varying/until patterns.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-I               PIC 9(5) VALUE 0.
       01  WS-J               PIC 9(5) VALUE 0.
       01  WS-ACCUM           PIC 9(10) VALUE 0.
       01  WS-OUTER-LIMIT     PIC 9(5) VALUE 1000.
       01  WS-INNER-LIMIT     PIC 9(5) VALUE 10000.
       PROCEDURE DIVISION.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-OUTER-LIMIT
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-INNER-LIMIT
                   ADD WS-J TO WS-ACCUM
               END-PERFORM
           END-PERFORM
           DISPLAY "ACCUM=" WS-ACCUM
           STOP RUN.
