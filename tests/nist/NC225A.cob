       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC225A.
      *
      * NIST CCVS-style test: STRING with multiple sources
      * DELIMITED BY SIZE. Tests that STRING concatenates
      * entire source fields when DELIMITED BY SIZE is used.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC1          PIC X(3) VALUE "ABC".
       01 WS-SRC2          PIC X(3) VALUE "DEF".
       01 WS-SRC3          PIC X(3) VALUE "GHI".
       01 WS-TARGET        PIC X(20) VALUE SPACES.
       01 WS-PTR           PIC 9(2) VALUE 1.
       01 WS-NUM-SRC       PIC X(4) VALUE "1234".
       01 WS-RESULT        PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: STRING three 3-char fields DELIMITED BY SIZE
      *   "ABC" + "DEF" + "GHI" => "ABCDEFGHI"
           MOVE SPACES TO WS-TARGET.
           MOVE 1 TO WS-PTR.
           STRING WS-SRC1 DELIMITED BY SIZE
                  WS-SRC2 DELIMITED BY SIZE
                  WS-SRC3 DELIMITED BY SIZE
             INTO WS-TARGET
             WITH POINTER WS-PTR.
           IF WS-TARGET(1:9) = "ABCDEFGHI"
               DISPLAY "NC225A-TEST-1 PASS"
           ELSE
               DISPLAY "NC225A-TEST-1 FAIL"
               DISPLAY "  Expected ABCDEFGHI, got >"
                   WS-TARGET "<"
           END-IF.
      * Test 2: STRING mixing variables and literals BY SIZE
      *   "ABC" + "-" + "1234" + "!" => "ABC-1234!"
           MOVE SPACES TO WS-RESULT.
           MOVE 1 TO WS-PTR.
           STRING WS-SRC1 DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-NUM-SRC DELIMITED BY SIZE
                  "!" DELIMITED BY SIZE
             INTO WS-RESULT
             WITH POINTER WS-PTR.
           IF WS-RESULT(1:9) = "ABC-1234!"
               DISPLAY "NC225A-TEST-2 PASS"
           ELSE
               DISPLAY "NC225A-TEST-2 FAIL"
               DISPLAY "  Expected ABC-1234!, got >"
                   WS-RESULT "<"
           END-IF.
           STOP RUN.
