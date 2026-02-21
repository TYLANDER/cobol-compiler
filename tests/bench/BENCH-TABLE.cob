       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCH-TABLE.
      *> Benchmark: table/array operations.
      *> Exercises indexed PERFORM with table lookups and SET.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TABLE.
           05  WS-ENTRY       PIC 9(5) OCCURS 1000 TIMES.
       01  WS-I               PIC 9(5) VALUE 0.
       01  WS-J               PIC 9(5) VALUE 0.
       01  WS-SUM             PIC 9(10) VALUE 0.
       01  WS-PASSES          PIC 9(4) VALUE 0.
       01  WS-PASS-LIMIT      PIC 9(4) VALUE 5000.
       PROCEDURE DIVISION.
      *>   Initialize table
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 1000
               MOVE WS-I TO WS-ENTRY(WS-I)
           END-PERFORM
      *>   Repeatedly sum the table
           PERFORM UNTIL WS-PASSES >= WS-PASS-LIMIT
               MOVE 0 TO WS-SUM
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > 1000
                   ADD WS-ENTRY(WS-I) TO WS-SUM
               END-PERFORM
               ADD 1 TO WS-PASSES
           END-PERFORM
           DISPLAY "SUM=" WS-SUM
           STOP RUN.
