       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC209A.
      *
      * NIST CCVS-style test: Level 88 with multiple VALUES and THRU
      * Tests level-88 condition names with multiple discrete values,
      * THRU ranges, and combined discrete values with ranges.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GRADE         PIC 9(3) VALUE ZEROS.
           88 GRADE-FAIL       VALUE 0 THRU 59.
           88 GRADE-PASS       VALUE 60 THRU 100.
           88 GRADE-A          VALUE 90 THRU 100.
           88 GRADE-B          VALUE 80 THRU 89.
       01 WS-DAY-CODE      PIC 9 VALUE 0.
           88 DAY-WEEKDAY      VALUE 1 2 3 4 5.
           88 DAY-WEEKEND      VALUE 6 7.
       01 WS-CATEGORY      PIC X(1) VALUE SPACE.
           88 CAT-VOWEL        VALUE "A" "E" "I" "O" "U".
           88 CAT-DIGIT        VALUE "0" THRU "9".
       PROCEDURE DIVISION.
      * Test 1: Level 88 with THRU range
      *   Grade 45 is FAIL, Grade 95 is PASS and A
           MOVE 45 TO WS-GRADE.
           IF GRADE-FAIL AND NOT GRADE-PASS
               MOVE 95 TO WS-GRADE
               IF GRADE-PASS AND GRADE-A
                   DISPLAY "NC209A-TEST-1 PASS"
               ELSE
                   DISPLAY "NC209A-TEST-1 FAIL"
                   DISPLAY "  95 not PASS/A, grade="
                       WS-GRADE
               END-IF
           ELSE
               DISPLAY "NC209A-TEST-1 FAIL"
               DISPLAY "  45 not FAIL, grade=" WS-GRADE
           END-IF.
      * Test 2: Level 88 with multiple discrete values
      *   Day 3 is weekday, Day 7 is weekend
           MOVE 3 TO WS-DAY-CODE.
           IF DAY-WEEKDAY AND NOT DAY-WEEKEND
               MOVE 7 TO WS-DAY-CODE
               IF DAY-WEEKEND AND NOT DAY-WEEKDAY
                   DISPLAY "NC209A-TEST-2 PASS"
               ELSE
                   DISPLAY "NC209A-TEST-2 FAIL"
                   DISPLAY "  7 not weekend"
               END-IF
           ELSE
               DISPLAY "NC209A-TEST-2 FAIL"
               DISPLAY "  3 not weekday"
           END-IF.
      * Test 3: Level 88 with discrete values on alphanumeric
      *   "E" is vowel, "5" is digit, "X" is neither
           MOVE "E" TO WS-CATEGORY.
           IF CAT-VOWEL AND NOT CAT-DIGIT
               MOVE "5" TO WS-CATEGORY
               IF CAT-DIGIT AND NOT CAT-VOWEL
                   MOVE "X" TO WS-CATEGORY
                   IF NOT CAT-VOWEL AND NOT CAT-DIGIT
                       DISPLAY "NC209A-TEST-3 PASS"
                   ELSE
                       DISPLAY "NC209A-TEST-3 FAIL"
                       DISPLAY "  X matched a category"
                   END-IF
               ELSE
                   DISPLAY "NC209A-TEST-3 FAIL"
                   DISPLAY "  5 not digit"
               END-IF
           ELSE
               DISPLAY "NC209A-TEST-3 FAIL"
               DISPLAY "  E not vowel"
           END-IF.
           STOP RUN.
