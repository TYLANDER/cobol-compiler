       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC400A.
      *
      * NIST CCVS-style test: Multiple WHEN in EVALUATE
      * Tests stacked WHEN on a subject value.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CODE            PIC 9(2)  VALUE 0.
       01 WS-TYPE            PIC X     VALUE SPACES.
       01 WS-RESULT          PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE with stacked WHEN matching "D"
           MOVE "D" TO WS-TYPE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-TYPE
               WHEN "A"
               WHEN "B"
               WHEN "C"
                   MOVE "GROUP1" TO WS-RESULT
               WHEN "D"
               WHEN "E"
                   MOVE "GROUP2" TO WS-RESULT
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "GROUP2    "
               DISPLAY "NC400A-TEST-1 PASS"
           ELSE
               DISPLAY "NC400A-TEST-1 FAIL"
               DISPLAY "  Expected GROUP2, got " WS-RESULT
           END-IF.
      * Test 2: EVALUATE with stacked WHEN matching "B"
           MOVE "B" TO WS-TYPE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-TYPE
               WHEN "A"
               WHEN "B"
               WHEN "C"
                   MOVE "FIRST" TO WS-RESULT
               WHEN "X"
               WHEN "Y"
               WHEN "Z"
                   MOVE "LAST" TO WS-RESULT
               WHEN OTHER
                   MOVE "UNKNOWN" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "FIRST     "
               DISPLAY "NC400A-TEST-2 PASS"
           ELSE
               DISPLAY "NC400A-TEST-2 FAIL"
               DISPLAY "  Expected FIRST, got " WS-RESULT
           END-IF.
      * Test 3: EVALUATE OTHER path
           MOVE "Q" TO WS-TYPE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-TYPE
               WHEN "A"
               WHEN "B"
                   MOVE "GROUP-1" TO WS-RESULT
               WHEN "C"
                   MOVE "GROUP-2" TO WS-RESULT
               WHEN OTHER
                   MOVE "DEFAULT" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "DEFAULT   "
               DISPLAY "NC400A-TEST-3 PASS"
           ELSE
               DISPLAY "NC400A-TEST-3 FAIL"
               DISPLAY "  Expected DEFAULT, got " WS-RESULT
           END-IF.
           STOP RUN.
