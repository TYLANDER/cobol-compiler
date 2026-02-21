       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCH-STRING.
      *> Benchmark: STRING/UNSTRING operations.
      *> Exercises string concatenation and splitting.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER         PIC 9(7) VALUE 0.
       01  WS-LIMIT           PIC 9(7) VALUE 1000000.
       01  WS-FIRST           PIC X(10) VALUE "JOHN      ".
       01  WS-LAST            PIC X(10) VALUE "DOE       ".
       01  WS-FULL-NAME       PIC X(25) VALUE SPACES.
       01  WS-PTR             PIC 99 VALUE 0.
       01  WS-OUT-FIRST       PIC X(10) VALUE SPACES.
       01  WS-OUT-LAST        PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
           PERFORM UNTIL WS-COUNTER >= WS-LIMIT
               MOVE 1 TO WS-PTR
               STRING WS-FIRST DELIMITED SPACE
                      ", " DELIMITED SIZE
                      WS-LAST DELIMITED SPACE
                   INTO WS-FULL-NAME
                   WITH POINTER WS-PTR
               END-STRING
               MOVE SPACES TO WS-OUT-FIRST
               MOVE SPACES TO WS-OUT-LAST
               UNSTRING WS-FULL-NAME DELIMITED ", "
                   INTO WS-OUT-FIRST WS-OUT-LAST
               END-UNSTRING
               ADD 1 TO WS-COUNTER
           END-PERFORM
           DISPLAY "NAME=" WS-FULL-NAME
           STOP RUN.
