       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC386A.
      *
      * NIST CCVS-style test: Numeric edited MOVE
      * Tests Z-suppression without comma or floating dollar
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1            PIC 9(5) VALUE 12345.
       01 WS-NUM2            PIC 9(5) VALUE 00042.
       01 WS-NUM3            PIC 9(5) VALUE 00000.
       01 WS-EDIT1           PIC ZZZZZ.
       01 WS-EDIT2           PIC ZZZZ9.
       01 WS-EDIT3           PIC ZZZZZ.
       PROCEDURE DIVISION.
      * Test 1: Z-suppression all significant digits
           MOVE WS-NUM1 TO WS-EDIT1.
           IF WS-EDIT1 = "12345"
               DISPLAY "NC386A-TEST-1 PASS"
           ELSE
               DISPLAY "NC386A-TEST-1 FAIL"
               DISPLAY "  Got [" WS-EDIT1 "]"
           END-IF.
      * Test 2: Z-suppression with leading zeros replaced by spaces
           MOVE WS-NUM2 TO WS-EDIT2.
           IF WS-EDIT2 = "   42"
               DISPLAY "NC386A-TEST-2 PASS"
           ELSE
               DISPLAY "NC386A-TEST-2 FAIL"
               DISPLAY "  Got [" WS-EDIT2 "]"
           END-IF.
      * Test 3: All zeros with all-Z pattern = all spaces
           MOVE WS-NUM3 TO WS-EDIT3.
           IF WS-EDIT3 = "     "
               DISPLAY "NC386A-TEST-3 PASS"
           ELSE
               DISPLAY "NC386A-TEST-3 FAIL"
               DISPLAY "  Got [" WS-EDIT3 "]"
           END-IF.
           STOP RUN.
