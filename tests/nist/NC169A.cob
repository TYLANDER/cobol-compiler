       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC169A.
      *
      * NIST CCVS-style test: Numeric edited MOVE
      * Tests MOVE of numeric values into numeric edited
      * picture fields (Z suppression, insertion chars, etc.)
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-AMOUNT1      PIC 9(5)V99 VALUE 1234.56.
       01 WS-AMOUNT2      PIC 9(5)V99 VALUE 7.89.
       01 WS-AMOUNT3      PIC 9(5)V99 VALUE 0.
       01 WS-EDIT1        PIC Z(4)9.99.
       01 WS-EDIT2        PIC Z(4)9.99.
       01 WS-EDIT3        PIC Z(4)9.99.
       PROCEDURE DIVISION.
      * Test 1: Numeric edit with leading zero suppression
      *   1234.56 => " 1234.56"
           MOVE WS-AMOUNT1 TO WS-EDIT1.
           IF WS-EDIT1 = " 1234.56"
               DISPLAY "NC169A-TEST-1 PASS"
           ELSE
               DISPLAY "NC169A-TEST-1 FAIL"
               DISPLAY "  Expected [ 1234.56] got ["
                   WS-EDIT1 "]"
           END-IF.
      * Test 2: Small value with many suppressed zeros
      *   7.89 => "    7.89"
           MOVE WS-AMOUNT2 TO WS-EDIT2.
           IF WS-EDIT2 = "    7.89"
               DISPLAY "NC169A-TEST-2 PASS"
           ELSE
               DISPLAY "NC169A-TEST-2 FAIL"
               DISPLAY "  Expected [    7.89] got ["
                   WS-EDIT2 "]"
           END-IF.
      * Test 3: Zero value with Z suppression
      *   0 => "    0.00"  (last digit always shown with 9)
           MOVE WS-AMOUNT3 TO WS-EDIT3.
           IF WS-EDIT3 = "    0.00"
               DISPLAY "NC169A-TEST-3 PASS"
           ELSE
               DISPLAY "NC169A-TEST-3 FAIL"
               DISPLAY "  Expected [    0.00] got ["
                   WS-EDIT3 "]"
           END-IF.
           STOP RUN.
