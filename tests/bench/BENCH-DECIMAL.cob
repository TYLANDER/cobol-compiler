       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCH-DECIMAL.
      *> Benchmark: decimal (fixed-point) arithmetic.
      *> Exercises COMPUTE with PIC V99 fractional values.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER         PIC 9(7) VALUE 0.
       01  WS-LIMIT           PIC 9(7) VALUE 5000000.
       01  WS-PRICE           PIC 9(5)V99 VALUE 100.50.
       01  WS-TAX-RATE        PIC V999 VALUE 0.075.
       01  WS-TAX             PIC 9(5)V99 VALUE 0.
       01  WS-TOTAL           PIC 9(8)V99 VALUE 0.
       01  WS-DISCOUNT        PIC 9(5)V99 VALUE 0.
       PROCEDURE DIVISION.
           PERFORM UNTIL WS-COUNTER >= WS-LIMIT
               COMPUTE WS-TAX = WS-PRICE * WS-TAX-RATE
               ADD WS-TAX TO WS-TOTAL
               COMPUTE WS-DISCOUNT = WS-PRICE * 0.10
               SUBTRACT WS-DISCOUNT FROM WS-TOTAL
               ADD 0.01 TO WS-PRICE
               ADD 1 TO WS-COUNTER
           END-PERFORM
           DISPLAY "TOTAL=" WS-TOTAL
           STOP RUN.
