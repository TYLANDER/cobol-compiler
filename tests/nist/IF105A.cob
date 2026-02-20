       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF105A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PADDED           PIC X(10) VALUE "  Hello   ".
       01  WS-RESULT           PIC X(10).
       PROCEDURE DIVISION.
      *    TEST 1: TRIM of literal removes leading/trailing spaces
           MOVE FUNCTION TRIM("  Hello  ")
               TO WS-RESULT
           IF WS-RESULT = "Hello     "
               DISPLAY "IF105A-TEST-1 PASS"
           ELSE
               DISPLAY "IF105A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: TRIM of a space-padded field
           MOVE FUNCTION TRIM(WS-PADDED)
               TO WS-RESULT
           IF WS-RESULT = "Hello     "
               DISPLAY "IF105A-TEST-2 PASS"
           ELSE
               DISPLAY "IF105A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
