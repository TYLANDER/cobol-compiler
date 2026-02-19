       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC253A.
      *
      * NIST CCVS-style test: SET condition-name (level-88) to TRUE
      * Tests SET cond-name TO TRUE and testing with IF.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STATUS       PIC 9 VALUE 0.
           88 WS-ACTIVE    VALUE 1.
           88 WS-INACTIVE  VALUE 0.
       01 WS-COLOR        PIC X VALUE SPACES.
           88 WS-RED       VALUE "R".
           88 WS-GREEN     VALUE "G".
           88 WS-BLUE      VALUE "B".
       01 WS-SWITCH       PIC 9 VALUE 0.
           88 WS-ON        VALUE 1.
           88 WS-OFF       VALUE 0.
       PROCEDURE DIVISION.
       NC253A-CONTROL.
           PERFORM NC253A-TEST-1.
           PERFORM NC253A-TEST-2.
           PERFORM NC253A-TEST-3.
           STOP RUN.
       NC253A-TEST-1.
      * SET WS-ACTIVE TO TRUE should set WS-STATUS to 1
           MOVE 0 TO WS-STATUS.
           SET WS-ACTIVE TO TRUE.
           IF WS-ACTIVE
               DISPLAY "NC253A-TEST-1 PASS"
           ELSE
               DISPLAY "NC253A-TEST-1 FAIL"
               DISPLAY "  WS-STATUS=" WS-STATUS
           END-IF.
       NC253A-TEST-2.
      * SET WS-GREEN TO TRUE should set WS-COLOR to "G"
           MOVE SPACES TO WS-COLOR.
           SET WS-GREEN TO TRUE.
           IF WS-GREEN AND WS-COLOR = "G"
               DISPLAY "NC253A-TEST-2 PASS"
           ELSE
               DISPLAY "NC253A-TEST-2 FAIL"
               DISPLAY "  WS-COLOR=" WS-COLOR
           END-IF.
       NC253A-TEST-3.
      * SET one 88 then another to verify switching
      * SET WS-ON then SET WS-OFF
           MOVE 0 TO WS-SWITCH.
           SET WS-ON TO TRUE.
           IF WS-ON
               SET WS-OFF TO TRUE
               IF WS-OFF AND WS-SWITCH = 0
                   DISPLAY "NC253A-TEST-3 PASS"
               ELSE
                   DISPLAY "NC253A-TEST-3 FAIL"
                   DISPLAY "  After OFF: WS-SWITCH="
                       WS-SWITCH
               END-IF
           ELSE
               DISPLAY "NC253A-TEST-3 FAIL"
               DISPLAY "  SET WS-ON failed, WS-SWITCH="
                   WS-SWITCH
           END-IF.
