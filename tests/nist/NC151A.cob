       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC151A.
      *
      * NIST CCVS-style test: Level-88 condition names (single VALUE)
      * Tests IF condition-name usage with level-88 items that have
      * single VALUE clauses, including negative and boundary checks.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COLOR         PIC X(1) VALUE SPACE.
           88 COLOR-RED        VALUE "R".
           88 COLOR-GREEN      VALUE "G".
           88 COLOR-BLUE       VALUE "B".
       01 WS-SWITCH         PIC 9    VALUE 0.
           88 SWITCH-ON        VALUE 1.
           88 SWITCH-OFF       VALUE 0.
       01 WS-RESULT         PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: IF condition-name true for matching single value
      *   MOVE "G" => COLOR-GREEN should be true
           MOVE "G" TO WS-COLOR.
           MOVE SPACES TO WS-RESULT.
           IF COLOR-GREEN
               MOVE "GREEN" TO WS-RESULT
           ELSE
               MOVE "NOTGREEN" TO WS-RESULT
           END-IF.
           IF WS-RESULT(1:5) = "GREEN"
               AND COLOR-GREEN
               AND NOT COLOR-RED
               AND NOT COLOR-BLUE
               DISPLAY "NC151A-TEST-1 PASS"
           ELSE
               DISPLAY "NC151A-TEST-1 FAIL"
               DISPLAY "  Expected GREEN, got >" WS-RESULT "<"
           END-IF.
      * Test 2: Condition-name false for non-matching value
      *   MOVE "X" => none of the conditions should be true
           MOVE "X" TO WS-COLOR.
           IF NOT COLOR-RED
               AND NOT COLOR-GREEN
               AND NOT COLOR-BLUE
               DISPLAY "NC151A-TEST-2 PASS"
           ELSE
               DISPLAY "NC151A-TEST-2 FAIL"
               DISPLAY "  No condition should be true for X"
           END-IF.
      * Test 3: Multiple 88-levels on numeric field
      *   SWITCH-OFF should be true when value is 0
      *   Then change to 1, SWITCH-ON should be true
           MOVE 0 TO WS-SWITCH.
           IF SWITCH-OFF
               MOVE 1 TO WS-SWITCH
               IF SWITCH-ON
                   DISPLAY "NC151A-TEST-3 PASS"
               ELSE
                   DISPLAY "NC151A-TEST-3 FAIL"
                   DISPLAY "  SWITCH-ON false after MOVE 1"
               END-IF
           ELSE
               DISPLAY "NC151A-TEST-3 FAIL"
               DISPLAY "  SWITCH-OFF false when value is 0"
           END-IF.
           STOP RUN.
