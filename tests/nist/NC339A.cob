       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC339A.
      *
      * NIST CCVS-style test: EVALUATE TRUE ALSO TRUE
      * (multi-subject EVALUATE with boolean conditions).
      * Tests EVALUATE with two boolean subjects separated by
      * ALSO, requiring both conditions to match in a WHEN clause.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GENDER        PIC X(1) VALUE SPACE.
       01 WS-AGE           PIC 9(3) VALUE 0.
       01 WS-RESULT        PIC X(15) VALUE SPACES.
       01 WS-DAY-TYPE      PIC X(7) VALUE SPACES.
       01 WS-SHIFT         PIC X(7) VALUE SPACES.
       01 WS-RATE          PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE TRUE ALSO TRUE - two conditions
      *   Gender "M" and age 25 => "YOUNG-MALE"
           MOVE "M" TO WS-GENDER.
           MOVE 25 TO WS-AGE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE ALSO TRUE
               WHEN WS-GENDER = "M" ALSO WS-AGE < 30
                   MOVE "YOUNG-MALE" TO WS-RESULT
               WHEN WS-GENDER = "M" ALSO WS-AGE > 29
                   MOVE "SENIOR-MALE" TO WS-RESULT
               WHEN WS-GENDER = "F" ALSO WS-AGE < 30
                   MOVE "YOUNG-FEMALE" TO WS-RESULT
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "YOUNG-MALE     "
               DISPLAY "NC339A-TEST-1 PASS"
           ELSE
               DISPLAY "NC339A-TEST-1 FAIL"
               DISPLAY "  Expected 'YOUNG-MALE     ', got '"
                   WS-RESULT "'"
           END-IF.
      * Test 2: EVALUATE TRUE ALSO TRUE - different match
      *   Gender "F" and age 20 => "YOUNG-FEMALE"
           MOVE "F" TO WS-GENDER.
           MOVE 20 TO WS-AGE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE ALSO TRUE
               WHEN WS-GENDER = "M" ALSO WS-AGE < 30
                   MOVE "YOUNG-MALE" TO WS-RESULT
               WHEN WS-GENDER = "M" ALSO WS-AGE > 29
                   MOVE "SENIOR-MALE" TO WS-RESULT
               WHEN WS-GENDER = "F" ALSO WS-AGE < 30
                   MOVE "YOUNG-FEMALE" TO WS-RESULT
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "YOUNG-FEMALE   "
               DISPLAY "NC339A-TEST-2 PASS"
           ELSE
               DISPLAY "NC339A-TEST-2 FAIL"
               DISPLAY "  Expected 'YOUNG-FEMALE   ', got '"
                   WS-RESULT "'"
           END-IF.
      * Test 3: EVALUATE with two literal subjects using ALSO
      *   Day "WEEKDAY" and shift "NIGHT" => rate 150
           MOVE "WEEKDAY" TO WS-DAY-TYPE.
           MOVE "NIGHT  " TO WS-SHIFT.
           MOVE 0 TO WS-RATE.
           EVALUATE WS-DAY-TYPE ALSO WS-SHIFT
               WHEN "WEEKDAY" ALSO "DAY    "
                   MOVE 100 TO WS-RATE
               WHEN "WEEKDAY" ALSO "NIGHT  "
                   MOVE 150 TO WS-RATE
               WHEN "WEEKEND" ALSO "DAY    "
                   MOVE 200 TO WS-RATE
               WHEN "WEEKEND" ALSO "NIGHT  "
                   MOVE 250 TO WS-RATE
               WHEN OTHER
                   MOVE 0 TO WS-RATE
           END-EVALUATE.
           IF WS-RATE = 150
               DISPLAY "NC339A-TEST-3 PASS"
           ELSE
               DISPLAY "NC339A-TEST-3 FAIL"
               DISPLAY "  Expected 150, got " WS-RATE
           END-IF.
           STOP RUN.
