       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC273A.
      *
      * NIST CCVS-style test: DIVIDE INTO (accumulator form)
      * Tests DIVIDE literal INTO identifier without GIVING,
      * which stores the quotient back in the identifier.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAL1       PIC 9(4) VALUE ZEROS.
       01 WS-VAL2       PIC 9(4) VALUE ZEROS.
       01 WS-VAL3       PIC 9(4) VALUE ZEROS.
       01 WS-REM        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC273A-CONTROL.
           PERFORM NC273A-TEST-1.
           PERFORM NC273A-TEST-2.
           PERFORM NC273A-TEST-3.
           STOP RUN.
       NC273A-TEST-1.
      * DIVIDE 5 INTO WS-VAL1
      *   WS-VAL1 starts at 100; 100 / 5 = 20
      *   Result stored back in WS-VAL1
           MOVE 100 TO WS-VAL1.
           DIVIDE 5 INTO WS-VAL1.
           IF WS-VAL1 = 20
               DISPLAY "NC273A-TEST-1 PASS"
           ELSE
               DISPLAY "NC273A-TEST-1 FAIL"
               DISPLAY "  Expected 20, got " WS-VAL1
           END-IF.
       NC273A-TEST-2.
      * DIVIDE identifier INTO identifier
      *   WS-VAL2 = 3, WS-VAL3 = 99; 99 / 3 = 33
      *   Result stored in WS-VAL3
           MOVE 3 TO WS-VAL2.
           MOVE 99 TO WS-VAL3.
           DIVIDE WS-VAL2 INTO WS-VAL3.
           IF WS-VAL3 = 33
               DISPLAY "NC273A-TEST-2 PASS"
           ELSE
               DISPLAY "NC273A-TEST-2 FAIL"
               DISPLAY "  Expected 33, got " WS-VAL3
           END-IF.
       NC273A-TEST-3.
      * Multiple DIVIDE INTO operations in sequence
      *   Start with 1000, divide by 10 => 100
      *   Then divide by 5 => 20
      *   Then divide by 4 => 5
           MOVE 1000 TO WS-VAL1.
           DIVIDE 10 INTO WS-VAL1.
           DIVIDE 5 INTO WS-VAL1.
           DIVIDE 4 INTO WS-VAL1.
           IF WS-VAL1 = 5
               DISPLAY "NC273A-TEST-3 PASS"
           ELSE
               DISPLAY "NC273A-TEST-3 FAIL"
               DISPLAY "  Expected 5, got " WS-VAL1
           END-IF.
