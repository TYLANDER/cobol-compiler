       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC264A.
      *
      * NIST CCVS-style test: Nested EVALUATE
      * EVALUATE inside EVALUATE's WHEN clause.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CATEGORY        PIC X(1)  VALUE SPACES.
       01 WS-VALUE            PIC 9(3)  VALUE ZEROS.
       01 WS-RESULT           PIC X(20) VALUE SPACES.
       01 WS-CODE             PIC 9(1)  VALUE ZEROS.
       01 WS-SUBCODE          PIC 9(1)  VALUE ZEROS.
       01 WS-LABEL            PIC X(15) VALUE SPACES.
       01 WS-DAY-NUM          PIC 9(1)  VALUE ZEROS.
       01 WS-SHIFT            PIC X(1)  VALUE SPACES.
       01 WS-SCHEDULE         PIC X(15) VALUE SPACES.
       PROCEDURE DIVISION.
       NC264A-CONTROL.
           PERFORM NC264A-TEST-1
           PERFORM NC264A-TEST-2
           PERFORM NC264A-TEST-3
           STOP RUN.
       NC264A-TEST-1.
      * Nested EVALUATE: category then value range
      *   Category "A" with value 50 => "A-MEDIUM"
           MOVE "A" TO WS-CATEGORY.
           MOVE 50 TO WS-VALUE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-CATEGORY
               WHEN "A"
                   EVALUATE TRUE
                       WHEN WS-VALUE < 25
                           MOVE "A-LOW" TO WS-RESULT
                       WHEN WS-VALUE < 75
                           MOVE "A-MEDIUM" TO WS-RESULT
                       WHEN OTHER
                           MOVE "A-HIGH" TO WS-RESULT
                   END-EVALUATE
               WHEN "B"
                   MOVE "B-ANY" TO WS-RESULT
               WHEN OTHER
                   MOVE "UNKNOWN" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "A-MEDIUM"
               DISPLAY "NC264A-TEST-1 PASS"
           ELSE
               DISPLAY "NC264A-TEST-1 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
           END-IF.
       NC264A-TEST-2.
      * Nested EVALUATE with numeric codes
      *   CODE=2 SUBCODE=3 => "TWO-HIGH"
           MOVE 2 TO WS-CODE.
           MOVE 3 TO WS-SUBCODE.
           MOVE SPACES TO WS-LABEL.
           EVALUATE WS-CODE
               WHEN 1
                   MOVE "ONE" TO WS-LABEL
               WHEN 2
                   EVALUATE WS-SUBCODE
                       WHEN 1
                           MOVE "TWO-LOW" TO WS-LABEL
                       WHEN 2
                           MOVE "TWO-MID" TO WS-LABEL
                       WHEN 3
                           MOVE "TWO-HIGH" TO WS-LABEL
                       WHEN OTHER
                           MOVE "TWO-OTHER" TO WS-LABEL
                   END-EVALUATE
               WHEN 3
                   MOVE "THREE" TO WS-LABEL
               WHEN OTHER
                   MOVE "UNKNOWN" TO WS-LABEL
           END-EVALUATE.
           IF WS-LABEL = "TWO-HIGH"
               DISPLAY "NC264A-TEST-2 PASS"
           ELSE
               DISPLAY "NC264A-TEST-2 FAIL"
               DISPLAY "  LABEL=" WS-LABEL
           END-IF.
       NC264A-TEST-3.
      * Nested EVALUATE: day and shift => schedule
      *   DAY=5 (weekday) SHIFT="M" => "WEEKDAY-MORN"
           MOVE 5 TO WS-DAY-NUM.
           MOVE "M" TO WS-SHIFT.
           MOVE SPACES TO WS-SCHEDULE.
           EVALUATE TRUE
               WHEN WS-DAY-NUM < 6
                   EVALUATE WS-SHIFT
                       WHEN "M"
                           MOVE "WEEKDAY-MORN" TO WS-SCHEDULE
                       WHEN "A"
                           MOVE "WEEKDAY-AFT" TO WS-SCHEDULE
                       WHEN OTHER
                           MOVE "WEEKDAY-EVE" TO WS-SCHEDULE
                   END-EVALUATE
               WHEN WS-DAY-NUM < 8
                   MOVE "WEEKEND" TO WS-SCHEDULE
               WHEN OTHER
                   MOVE "INVALID" TO WS-SCHEDULE
           END-EVALUATE.
           IF WS-SCHEDULE = "WEEKDAY-MORN"
               DISPLAY "NC264A-TEST-3 PASS"
           ELSE
               DISPLAY "NC264A-TEST-3 FAIL"
               DISPLAY "  SCHEDULE=" WS-SCHEDULE
           END-IF.
