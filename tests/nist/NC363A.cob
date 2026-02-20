       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC363A.
      *
      * NIST CCVS-style test: Group and elementary item
      * overlapping access patterns (group MOVE slicing).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RECORD.
           05 WS-FIRST    PIC X(5).
           05 WS-MIDDLE   PIC X(3).
           05 WS-LAST     PIC X(7).
       01 WS-FULL-CHECK  PIC X(15) VALUE SPACES.
       01 WS-FRONT-CHECK PIC X(8)  VALUE SPACES.
       PROCEDURE DIVISION.
      * Set up the record fields
           MOVE "JAMES" TO WS-FIRST.
           MOVE "T. " TO WS-MIDDLE.
           MOVE "KIRK   " TO WS-LAST.
      * Test 1: Group MOVE copies all subordinate fields
           MOVE WS-RECORD TO WS-FULL-CHECK.
           IF WS-FULL-CHECK = "JAMEST. KIRK   "
               DISPLAY "NC363A-TEST-1 PASS"
           ELSE
               DISPLAY "NC363A-TEST-1 FAIL"
               DISPLAY "  Expected JAMEST. KIRK   "
               DISPLAY "  Got      " WS-FULL-CHECK
           END-IF.
      * Test 2: Verify individual fields are correct
           IF WS-FIRST = "JAMES" AND WS-MIDDLE = "T. "
               AND WS-LAST = "KIRK   "
               DISPLAY "NC363A-TEST-2 PASS"
           ELSE
               DISPLAY "NC363A-TEST-2 FAIL"
               DISPLAY "  FIRST="  WS-FIRST
               DISPLAY "  MIDDLE=" WS-MIDDLE
               DISPLAY "  LAST="   WS-LAST
           END-IF.
      * Test 3: MOVE to group overwrites all subordinate fields
           MOVE "JOHN Q. PICARD " TO WS-RECORD.
           IF WS-FIRST = "JOHN " AND WS-MIDDLE = "Q. "
               AND WS-LAST = "PICARD "
               DISPLAY "NC363A-TEST-3 PASS"
           ELSE
               DISPLAY "NC363A-TEST-3 FAIL"
               DISPLAY "  FIRST="  WS-FIRST
               DISPLAY "  MIDDLE=" WS-MIDDLE
               DISPLAY "  LAST="   WS-LAST
           END-IF.
           STOP RUN.
