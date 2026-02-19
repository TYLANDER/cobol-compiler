       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC270A.
      *
      * NIST CCVS-style test: Alphanumeric comparison rules
      * Shorter field padded with spaces on the right for
      * comparison purposes.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SHORT          PIC X(3)  VALUE SPACES.
       01 WS-LONG           PIC X(10) VALUE SPACES.
       01 WS-A              PIC X(5)  VALUE SPACES.
       01 WS-B              PIC X(8)  VALUE SPACES.
       01 WS-X              PIC X(4)  VALUE SPACES.
       01 WS-Y              PIC X(4)  VALUE SPACES.
       PROCEDURE DIVISION.
       NC270A-CONTROL.
           PERFORM NC270A-TEST-1
           PERFORM NC270A-TEST-2
           PERFORM NC270A-TEST-3
           STOP RUN.
       NC270A-TEST-1.
      * Short field compared to long field
      *   "ABC" in X(3) should equal "ABC       " in X(10)
      *   COBOL pads shorter field with spaces for comparison
           MOVE "ABC" TO WS-SHORT.
           MOVE "ABC" TO WS-LONG.
           IF WS-SHORT = WS-LONG
               DISPLAY "NC270A-TEST-1 PASS"
           ELSE
               DISPLAY "NC270A-TEST-1 FAIL"
               DISPLAY "  SHORT=>" WS-SHORT "<"
               DISPLAY "  LONG=>" WS-LONG "<"
           END-IF.
       NC270A-TEST-2.
      * Short field with trailing content vs long field
      *   "HELLO" in X(5) compared to "HELLO   " in X(8)
      *   They should be equal (trailing spaces don't matter)
           MOVE "HELLO" TO WS-A.
           MOVE "HELLO" TO WS-B.
           IF WS-A = WS-B
               DISPLAY "NC270A-TEST-2 PASS"
           ELSE
               DISPLAY "NC270A-TEST-2 FAIL"
               DISPLAY "  A=>" WS-A "<"
               DISPLAY "  B=>" WS-B "<"
           END-IF.
       NC270A-TEST-3.
      * Fields with same size but different content
      *   "ABCD" should be less than "ABCE" (D < E)
           MOVE "ABCD" TO WS-X.
           MOVE "ABCE" TO WS-Y.
           IF WS-X < WS-Y
               DISPLAY "NC270A-TEST-3 PASS"
           ELSE
               DISPLAY "NC270A-TEST-3 FAIL"
               DISPLAY "  X=>" WS-X "<"
               DISPLAY "  Y=>" WS-Y "<"
           END-IF.
