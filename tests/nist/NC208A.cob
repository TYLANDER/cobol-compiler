       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC208A.
      *
      * NIST CCVS-style test: Numeric edited display formatting
      * Tests MOVE from numeric to numeric-edited fields with
      * various PIC editing symbols (Z, *, .).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM-SRC      PIC 9(5)V99 VALUE ZEROS.
       01 WS-EDIT1        PIC Z(4)9.99.
       01 WS-EDIT2        PIC *(5)9.99.
       01 WS-EDIT3        PIC Z(5)9.
       PROCEDURE DIVISION.
      * Test 1: Numeric to Z-edited with decimal
      *   12345.67 => Z(4)9.99 => "12345.67"
           MOVE 12345 TO WS-NUM-SRC.
           ADD 0.67 TO WS-NUM-SRC.
           MOVE WS-NUM-SRC TO WS-EDIT1.
           IF WS-EDIT1 = "12345.67"
               DISPLAY "NC208A-TEST-1 PASS"
           ELSE
               DISPLAY "NC208A-TEST-1 FAIL"
               DISPLAY "  Expected [12345.67] got ["
                   WS-EDIT1 "]"
           END-IF.
      * Test 2: Small value with asterisk fill
      *   42.50 => *(5)9.99 => "****42.50"
           MOVE 42 TO WS-NUM-SRC.
           ADD 0.50 TO WS-NUM-SRC.
           MOVE WS-NUM-SRC TO WS-EDIT2.
           IF WS-EDIT2 = "****42.50"
               DISPLAY "NC208A-TEST-2 PASS"
           ELSE
               DISPLAY "NC208A-TEST-2 FAIL"
               DISPLAY "  Expected [****42.50] got ["
                   WS-EDIT2 "]"
           END-IF.
      * Test 3: Integer with Z-suppression
      *   750 => Z(5)9 => "   750"
           MOVE 750 TO WS-NUM-SRC.
           MOVE WS-NUM-SRC TO WS-EDIT3.
           IF WS-EDIT3 = "   750"
               DISPLAY "NC208A-TEST-3 PASS"
           ELSE
               DISPLAY "NC208A-TEST-3 FAIL"
               DISPLAY "  Expected [   750] got ["
                   WS-EDIT3 "]"
           END-IF.
           STOP RUN.
