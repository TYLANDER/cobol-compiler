       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC388A.
      *
      * NIST CCVS-style test: Level-88 condition names with VALUES
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STATUS          PIC 9 VALUE 0.
           88 STATUS-OFF      VALUE 0.
           88 STATUS-ON       VALUE 1.
           88 STATUS-ACTIVE   VALUE 1 2 3.
       01 WS-CODE            PIC X VALUE SPACES.
           88 CODE-ALPHA      VALUE "A" "B" "C".
           88 CODE-NUMERIC    VALUE "1" "2" "3".
       PROCEDURE DIVISION.
      * Test 1: Level-88 single value
           MOVE 0 TO WS-STATUS.
           IF STATUS-OFF
               DISPLAY "NC388A-TEST-1 PASS"
           ELSE
               DISPLAY "NC388A-TEST-1 FAIL"
               DISPLAY "  STATUS-OFF should be true"
           END-IF.
      * Test 2: Level-88 multiple values
           MOVE 2 TO WS-STATUS.
           IF STATUS-ACTIVE
               DISPLAY "NC388A-TEST-2 PASS"
           ELSE
               DISPLAY "NC388A-TEST-2 FAIL"
               DISPLAY "  STATUS-ACTIVE should be true for 2"
           END-IF.
      * Test 3: Level-88 with alphanumeric values
           MOVE "B" TO WS-CODE.
           IF CODE-ALPHA AND NOT CODE-NUMERIC
               DISPLAY "NC388A-TEST-3 PASS"
           ELSE
               DISPLAY "NC388A-TEST-3 FAIL"
               DISPLAY "  CODE-ALPHA should be true for B"
           END-IF.
           STOP RUN.
