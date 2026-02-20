       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC353A.
      *
      * NIST CCVS-style test: UNSTRING with multiple DELIMITED BY
      * clauses using OR.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE       PIC X(30) VALUE SPACES.
       01 WS-FIELD-1      PIC X(10) VALUE SPACES.
       01 WS-FIELD-2      PIC X(10) VALUE SPACES.
       01 WS-FIELD-3      PIC X(10) VALUE SPACES.
       01 WS-DELIM-1      PIC X(1) VALUE SPACE.
       01 WS-DELIM-2      PIC X(1) VALUE SPACE.
       01 WS-CNT-1        PIC 99 VALUE 0.
       01 WS-CNT-2        PIC 99 VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: UNSTRING DELIMITED BY "," OR ";"
           MOVE "ABC,DEF;GHI" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD-1 WS-FIELD-2 WS-FIELD-3.
           UNSTRING WS-SOURCE DELIMITED BY "," OR ";"
               INTO WS-FIELD-1 WS-FIELD-2 WS-FIELD-3.
           IF WS-FIELD-1 = "ABC"
               AND WS-FIELD-2 = "DEF"
               AND WS-FIELD-3 = "GHI"
               DISPLAY "NC353A-TEST-1 PASS"
           ELSE
               DISPLAY "NC353A-TEST-1 FAIL"
               DISPLAY "  F1=" WS-FIELD-1
               DISPLAY "  F2=" WS-FIELD-2
               DISPLAY "  F3=" WS-FIELD-3
           END-IF.
      * Test 2: UNSTRING with DELIMITER IN
           MOVE "AAA/BBB-CCC" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD-1 WS-FIELD-2 WS-FIELD-3.
           MOVE SPACES TO WS-DELIM-1 WS-DELIM-2.
