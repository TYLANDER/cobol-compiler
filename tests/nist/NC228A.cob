       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC228A.
      *
      * NIST CCVS-style test: Level 88 with THRU ranges and
      * SET TO TRUE. Tests that SET condition-name TO TRUE
      * moves the first value of the 88-level to the parent,
      * including for THRU ranges and discrete values.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TEMP          PIC 9(3) VALUE 0.
           88 TEMP-COLD        VALUE 0 THRU 32.
           88 TEMP-WARM        VALUE 33 THRU 75.
           88 TEMP-HOT         VALUE 76 THRU 120.
       01 WS-COLOR         PIC X(1) VALUE SPACE.
           88 COLOR-RED        VALUE "R".
           88 COLOR-GREEN      VALUE "G".
           88 COLOR-BLUE       VALUE "B".
       PROCEDURE DIVISION.
      * Test 1: SET THRU-range condition TO TRUE sets first value
      *   SET TEMP-WARM TO TRUE => WS-TEMP = 33 (first of range)
           MOVE 0 TO WS-TEMP.
           SET TEMP-WARM TO TRUE.
           IF TEMP-WARM AND WS-TEMP = 33
               DISPLAY "NC228A-TEST-1 PASS"
           ELSE
               DISPLAY "NC228A-TEST-1 FAIL"
               DISPLAY "  Expected 33, got " WS-TEMP
           END-IF.
      * Test 2: SET discrete-value condition TO TRUE
      *   SET COLOR-BLUE TO TRUE => WS-COLOR = "B"
           MOVE SPACE TO WS-COLOR.
           SET COLOR-BLUE TO TRUE.
           IF COLOR-BLUE AND WS-COLOR = "B"
               DISPLAY "NC228A-TEST-2 PASS"
           ELSE
               DISPLAY "NC228A-TEST-2 FAIL"
               DISPLAY "  Expected B, got >" WS-COLOR "<"
           END-IF.
      * Test 3: SET two conditions in sequence, verify last wins
      *   SET TEMP-HOT then SET TEMP-COLD
           SET TEMP-HOT TO TRUE.
           SET TEMP-COLD TO TRUE.
           IF TEMP-COLD AND WS-TEMP = 0
               DISPLAY "NC228A-TEST-3 PASS"
           ELSE
               DISPLAY "NC228A-TEST-3 FAIL"
               DISPLAY "  Expected 0 (COLD), got " WS-TEMP
           END-IF.
           STOP RUN.
