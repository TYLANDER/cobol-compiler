       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC152A.
      *
      * NIST CCVS-style test: Level-88 condition names with VALUE THRU
      * Tests 88-level range conditions with VALUE THRU including
      * boundary values and multiple ranges.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TEMP          PIC S9(3) VALUE ZEROS.
           88 TEMP-COLD       VALUE -50 THRU 0.
           88 TEMP-COOL       VALUE 1 THRU 15.
           88 TEMP-WARM       VALUE 16 THRU 30.
           88 TEMP-HOT        VALUE 31 THRU 50.
       01 WS-AGE           PIC 9(3) VALUE ZEROS.
           88 AGE-CHILD       VALUE 0 THRU 12.
           88 AGE-TEEN        VALUE 13 THRU 19.
           88 AGE-ADULT       VALUE 20 THRU 999.
       PROCEDURE DIVISION.
      * Test 1: VALUE THRU — value at lower boundary
      *   MOVE 16 => TEMP-WARM should be true (lower bound)
      *   TEMP-COOL should be false
           MOVE 16 TO WS-TEMP.
           IF TEMP-WARM
               AND NOT TEMP-COOL
               AND NOT TEMP-HOT
               DISPLAY "NC152A-TEST-1 PASS"
           ELSE
               DISPLAY "NC152A-TEST-1 FAIL"
               DISPLAY "  TEMP=16, expected WARM"
               DISPLAY "  WS-TEMP=" WS-TEMP
           END-IF.
      * Test 2: VALUE THRU — value at upper boundary
      *   MOVE 30 => TEMP-WARM should be true (upper bound)
           MOVE 30 TO WS-TEMP.
           IF TEMP-WARM
               AND NOT TEMP-HOT
               DISPLAY "NC152A-TEST-2 PASS"
           ELSE
               DISPLAY "NC152A-TEST-2 FAIL"
               DISPLAY "  TEMP=30, expected WARM"
           END-IF.
      * Test 3: Multiple THRU ranges, verify transitions
      *   Child at 12, then teen at 13, then adult at 20
           MOVE 12 TO WS-AGE.
           IF AGE-CHILD
               MOVE 13 TO WS-AGE
               IF AGE-TEEN
                   MOVE 20 TO WS-AGE
                   IF AGE-ADULT
                       DISPLAY "NC152A-TEST-3 PASS"
                   ELSE
                       DISPLAY "NC152A-TEST-3 FAIL"
                       DISPLAY "  AGE=20 not ADULT"
                   END-IF
               ELSE
                   DISPLAY "NC152A-TEST-3 FAIL"
                   DISPLAY "  AGE=13 not TEEN"
               END-IF
           ELSE
               DISPLAY "NC152A-TEST-3 FAIL"
               DISPLAY "  AGE=12 not CHILD"
           END-IF.
           STOP RUN.
