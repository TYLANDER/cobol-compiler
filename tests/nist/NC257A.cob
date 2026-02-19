       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC257A.
      *
      * NIST CCVS-style test: REDEFINES with different types
      * Tests alphanumeric REDEFINES numeric, accessing both views.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM-FIELD    PIC 9(6) VALUE 123456.
       01 WS-ALPHA-VIEW REDEFINES WS-NUM-FIELD PIC X(6).
       01 WS-GROUP-REC.
           05 WS-PART-A   PIC X(4) VALUE "ABCD".
           05 WS-PART-B   PIC X(4) VALUE "1234".
       01 WS-FULL-VIEW REDEFINES WS-GROUP-REC PIC X(8).
       01 WS-NUM-REC.
           05 WS-HI       PIC 9(3) VALUE 100.
           05 WS-LO       PIC 9(3) VALUE 200.
       01 WS-COMBINED REDEFINES WS-NUM-REC PIC 9(6).
       PROCEDURE DIVISION.
       NC257A-CONTROL.
           PERFORM NC257A-TEST-1.
           PERFORM NC257A-TEST-2.
           PERFORM NC257A-TEST-3.
           STOP RUN.
       NC257A-TEST-1.
      * Numeric 123456 viewed as alphanumeric should be "123456"
           MOVE 123456 TO WS-NUM-FIELD.
           IF WS-ALPHA-VIEW = "123456"
               DISPLAY "NC257A-TEST-1 PASS"
           ELSE
               DISPLAY "NC257A-TEST-1 FAIL"
               DISPLAY "  Got >" WS-ALPHA-VIEW "<"
           END-IF.
       NC257A-TEST-2.
      * Group with two parts REDEFINES as single alphanumeric
      * "ABCD" + "1234" = "ABCD1234"
           MOVE "ABCD" TO WS-PART-A.
           MOVE "1234" TO WS-PART-B.
           IF WS-FULL-VIEW = "ABCD1234"
               DISPLAY "NC257A-TEST-2 PASS"
           ELSE
               DISPLAY "NC257A-TEST-2 FAIL"
               DISPLAY "  Got >" WS-FULL-VIEW "<"
           END-IF.
       NC257A-TEST-3.
      * Two numeric parts viewed as one combined number
      * HI=100, LO=200 => combined = 100200
           MOVE 100 TO WS-HI.
           MOVE 200 TO WS-LO.
           IF WS-COMBINED = 100200
               DISPLAY "NC257A-TEST-3 PASS"
           ELSE
               DISPLAY "NC257A-TEST-3 FAIL"
               DISPLAY "  Expected 100200, got "
                   WS-COMBINED
           END-IF.
