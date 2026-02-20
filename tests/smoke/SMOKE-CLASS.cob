       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-CLASS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM       PIC X(5) VALUE "12345".
       01 WS-ALPHA      PIC X(5) VALUE "ABCDE".
       01 WS-LOWER      PIC X(5) VALUE "abcde".
       01 WS-UPPER      PIC X(5) VALUE "ABCDE".
       01 WS-MIXED      PIC X(5) VALUE "Ab3dE".
       01 WS-SPACES     PIC X(5) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test IS NUMERIC
           IF WS-NUM IS NUMERIC
               DISPLAY "NUM-NUMERIC: PASS"
           ELSE
               DISPLAY "NUM-NUMERIC: FAIL"
           END-IF.
           IF WS-ALPHA IS NUMERIC
               DISPLAY "ALPHA-NUMERIC: FAIL"
           ELSE
               DISPLAY "ALPHA-NUMERIC: PASS"
           END-IF.
      * Test IS ALPHABETIC
           IF WS-ALPHA IS ALPHABETIC
               DISPLAY "ALPHA-ALPHA: PASS"
           ELSE
               DISPLAY "ALPHA-ALPHA: FAIL"
           END-IF.
           IF WS-NUM IS ALPHABETIC
               DISPLAY "NUM-ALPHA: FAIL"
           ELSE
               DISPLAY "NUM-ALPHA: PASS"
           END-IF.
           IF WS-SPACES IS ALPHABETIC
               DISPLAY "SPACES-ALPHA: PASS"
           ELSE
               DISPLAY "SPACES-ALPHA: FAIL"
           END-IF.
      * Test IS ALPHABETIC-LOWER
           IF WS-LOWER IS ALPHABETIC-LOWER
               DISPLAY "LOWER-LOWER: PASS"
           ELSE
               DISPLAY "LOWER-LOWER: FAIL"
           END-IF.
           IF WS-UPPER IS ALPHABETIC-LOWER
               DISPLAY "UPPER-LOWER: FAIL"
           ELSE
               DISPLAY "UPPER-LOWER: PASS"
           END-IF.
           IF WS-SPACES IS ALPHABETIC-LOWER
               DISPLAY "SPACES-LOWER: PASS"
           ELSE
               DISPLAY "SPACES-LOWER: FAIL"
           END-IF.
      * Test IS ALPHABETIC-UPPER
           IF WS-UPPER IS ALPHABETIC-UPPER
               DISPLAY "UPPER-UPPER: PASS"
           ELSE
               DISPLAY "UPPER-UPPER: FAIL"
           END-IF.
           IF WS-LOWER IS ALPHABETIC-UPPER
               DISPLAY "LOWER-UPPER: FAIL"
           ELSE
               DISPLAY "LOWER-UPPER: PASS"
           END-IF.
           IF WS-SPACES IS ALPHABETIC-UPPER
               DISPLAY "SPACES-UPPER: PASS"
           ELSE
               DISPLAY "SPACES-UPPER: FAIL"
           END-IF.
      * Test NOT conditions
           IF WS-ALPHA IS NOT NUMERIC
               DISPLAY "NOT-NUMERIC: PASS"
           ELSE
               DISPLAY "NOT-NUMERIC: FAIL"
           END-IF.
           IF WS-NUM IS NOT ALPHABETIC
               DISPLAY "NOT-ALPHA: PASS"
           ELSE
               DISPLAY "NOT-ALPHA: FAIL"
           END-IF.
           STOP RUN.
