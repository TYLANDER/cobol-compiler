       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCH-COMPARE.
      *> Benchmark: conditional logic and comparisons.
      *> Exercises IF/EVALUATE with numeric and alphanumeric comparisons.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER         PIC 9(8) VALUE 0.
       01  WS-LIMIT           PIC 9(8) VALUE 10000000.
       01  WS-VALUE           PIC 9(5) VALUE 0.
       01  WS-CATEGORY-A      PIC 9(8) VALUE 0.
       01  WS-CATEGORY-B      PIC 9(8) VALUE 0.
       01  WS-CATEGORY-C      PIC 9(8) VALUE 0.
       01  WS-CATEGORY-D      PIC 9(8) VALUE 0.
       PROCEDURE DIVISION.
           PERFORM UNTIL WS-COUNTER >= WS-LIMIT
               COMPUTE WS-VALUE =
                   FUNCTION MOD(WS-COUNTER, 100)
               EVALUATE TRUE
                   WHEN WS-VALUE < 25
                       ADD 1 TO WS-CATEGORY-A
                   WHEN WS-VALUE < 50
                       ADD 1 TO WS-CATEGORY-B
                   WHEN WS-VALUE < 75
                       ADD 1 TO WS-CATEGORY-C
                   WHEN OTHER
                       ADD 1 TO WS-CATEGORY-D
               END-EVALUATE
               ADD 1 TO WS-COUNTER
           END-PERFORM
           DISPLAY "A=" WS-CATEGORY-A
           DISPLAY "B=" WS-CATEGORY-B
           DISPLAY "C=" WS-CATEGORY-C
           DISPLAY "D=" WS-CATEGORY-D
           STOP RUN.
