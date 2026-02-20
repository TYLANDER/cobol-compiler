       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC397A.
      *
      * NIST CCVS-style test: Reference modification
      * Uses literal start and length only (no variable length).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA            PIC X(10) VALUE "ABCDEFGHIJ".
       01 WS-RESULT          PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Reference modification start=1, length=full
           MOVE WS-DATA(1:10) TO WS-RESULT.
           IF WS-RESULT = "ABCDEFGHIJ"
               DISPLAY "NC397A-TEST-1 PASS"
           ELSE
               DISPLAY "NC397A-TEST-1 FAIL"
               DISPLAY "  Got [" WS-RESULT "]"
           END-IF.
      * Test 2: Extract middle substring
           MOVE SPACES TO WS-RESULT.
           MOVE WS-DATA(4:3) TO WS-RESULT.
           IF WS-RESULT = "DEF       "
               DISPLAY "NC397A-TEST-2 PASS"
           ELSE
               DISPLAY "NC397A-TEST-2 FAIL"
               DISPLAY "  Got [" WS-RESULT "]"
           END-IF.
      * Test 3: Extract last character
           MOVE SPACES TO WS-RESULT.
           MOVE WS-DATA(10:1) TO WS-RESULT.
           IF WS-RESULT = "J         "
               DISPLAY "NC397A-TEST-3 PASS"
           ELSE
               DISPLAY "NC397A-TEST-3 FAIL"
               DISPLAY "  Got [" WS-RESULT "]"
           END-IF.
           STOP RUN.
