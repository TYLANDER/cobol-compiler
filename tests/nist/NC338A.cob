       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC338A.
      *
      * NIST CCVS-style test: Level-88 condition names with
      * multiple VALUES and THRU ranges (advanced cases).
      * Tests SET statement with level-88, alphanumeric 88's,
      * and boundary conditions on THRU ranges.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GRADE         PIC X(1) VALUE SPACE.
          88 WS-PASSING     VALUE "A" "B" "C".
          88 WS-FAILING     VALUE "D" "F".
          88 WS-HONORS      VALUE "A".
       01 WS-TEMP           PIC 9(3) VALUE 0.
          88 WS-COOL        VALUE 1 THRU 15.
          88 WS-NORMAL      VALUE 16 THRU 37.
          88 WS-FEVER       VALUE 38 THRU 42.
          88 WS-CRITICAL    VALUE 43 THRU 99.
       01 WS-AGE            PIC 9(3) VALUE 0.
          88 WS-CHILD       VALUE 0 THRU 12.
          88 WS-TEEN        VALUE 13 THRU 19.
          88 WS-ADULT       VALUE 20 THRU 64.
          88 WS-SENIOR      VALUE 65 THRU 120.
       PROCEDURE DIVISION.
      * Test 1: Level-88 with alphanumeric VALUES
      *   Setting WS-GRADE to "B" should make WS-PASSING true
      *   and WS-FAILING false.
           MOVE "B" TO WS-GRADE.
           IF WS-PASSING AND NOT WS-FAILING
               DISPLAY "NC338A-TEST-1 PASS"
           ELSE
               DISPLAY "NC338A-TEST-1 FAIL"
               DISPLAY "  Grade 'B': PASSING should be true,"
               DISPLAY "  FAILING should be false"
           END-IF.
      * Test 2: Level-88 with numeric THRU ranges
      *   Setting WS-TEMP to 5 should make WS-COOL true,
      *   WS-NORMAL false. Then change to 37 (boundary of NORMAL).
           MOVE 5 TO WS-TEMP.
           IF WS-COOL AND NOT WS-NORMAL
               MOVE 37 TO WS-TEMP
               IF WS-NORMAL AND NOT WS-FEVER
                   DISPLAY "NC338A-TEST-2 PASS"
               ELSE
                   DISPLAY "NC338A-TEST-2 FAIL"
                   DISPLAY "  Temp 37: NORMAL should be true"
               END-IF
           ELSE
               DISPLAY "NC338A-TEST-2 FAIL"
               DISPLAY "  Temp 5: COOL should be true"
           END-IF.
      * Test 3: Level-88 THRU range boundary values
      *   Age 19 is TEEN, age 20 is ADULT (exact boundaries)
           MOVE 19 TO WS-AGE.
           IF WS-TEEN AND NOT WS-ADULT
               MOVE 20 TO WS-AGE
               IF WS-ADULT AND NOT WS-TEEN
                   DISPLAY "NC338A-TEST-3 PASS"
               ELSE
                   DISPLAY "NC338A-TEST-3 FAIL"
                   DISPLAY "  Age 20: ADULT should be true"
               END-IF
           ELSE
               DISPLAY "NC338A-TEST-3 FAIL"
               DISPLAY "  Age 19: TEEN should be true"
           END-IF.
           STOP RUN.
