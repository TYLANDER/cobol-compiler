       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCH-ARITH.
      *> Benchmark: integer arithmetic loop (ADD, SUBTRACT, MULTIPLY).
      *> Loops 10 million times performing basic arithmetic.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER         PIC 9(8) VALUE 0.
       01  WS-LIMIT           PIC 9(8) VALUE 10000000.
       01  WS-ACCUM           PIC 9(10) VALUE 0.
       01  WS-TEMP            PIC 9(10) VALUE 0.
       01  WS-RESULT          PIC 9(10) VALUE 0.
       PROCEDURE DIVISION.
           PERFORM UNTIL WS-COUNTER >= WS-LIMIT
               ADD 7 TO WS-ACCUM
               SUBTRACT 3 FROM WS-ACCUM
               MULTIPLY WS-COUNTER BY 2 GIVING WS-TEMP
               ADD WS-TEMP TO WS-RESULT
               ADD 1 TO WS-COUNTER
           END-PERFORM
           DISPLAY "ACCUM=" WS-ACCUM
           DISPLAY "RESULT=" WS-RESULT
           STOP RUN.
