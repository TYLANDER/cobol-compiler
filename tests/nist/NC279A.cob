       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC279A.
      *
      * NIST CCVS-style test: EVALUATE with literal values
      * Tests EVALUATE with a numeric or alphanumeric subject
      * matched against literal WHEN values (not TRUE/FALSE).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CODE        PIC 9(3)  VALUE ZEROS.
       01 WS-RESULT      PIC X(10) VALUE SPACES.
       01 WS-LETTER      PIC X     VALUE SPACES.
       PROCEDURE DIVISION.
       NC279A-CONTROL.
           PERFORM NC279A-TEST-1.
           PERFORM NC279A-TEST-2.
           PERFORM NC279A-TEST-3.
           STOP RUN.
       NC279A-TEST-1.
      * EVALUATE numeric subject against literal values
      *   CODE=200 should match WHEN 200
           MOVE 200 TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-CODE
               WHEN 100
                   MOVE "HUNDRED" TO WS-RESULT
               WHEN 200
                   MOVE "TWO-HUND" TO WS-RESULT
               WHEN 300
                   MOVE "THREE-H" TO WS-RESULT
               WHEN OTHER
                   MOVE "UNKNOWN" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "TWO-HUND  "
               DISPLAY "NC279A-TEST-1 PASS"
           ELSE
               DISPLAY "NC279A-TEST-1 FAIL"
               DISPLAY "  Expected TWO-HUND, got >"
                   WS-RESULT "<"
           END-IF.
       NC279A-TEST-2.
      * EVALUATE alphanumeric subject against literal strings
      *   LETTER="B" should match WHEN "B"
           MOVE "B" TO WS-LETTER.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-LETTER
               WHEN "A"
                   MOVE "ALPHA" TO WS-RESULT
               WHEN "B"
                   MOVE "BRAVO" TO WS-RESULT
               WHEN "C"
                   MOVE "CHARLIE" TO WS-RESULT
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "BRAVO     "
               DISPLAY "NC279A-TEST-2 PASS"
           ELSE
               DISPLAY "NC279A-TEST-2 FAIL"
               DISPLAY "  Expected BRAVO, got >"
                   WS-RESULT "<"
           END-IF.
       NC279A-TEST-3.
      * EVALUATE with WHEN OTHER fallthrough
      *   CODE=999 matches no literal, falls to OTHER
           MOVE 999 TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-CODE
               WHEN 1
                   MOVE "ONE" TO WS-RESULT
               WHEN 2
                   MOVE "TWO" TO WS-RESULT
               WHEN 3
                   MOVE "THREE" TO WS-RESULT
               WHEN OTHER
                   MOVE "DEFAULT" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "DEFAULT   "
               DISPLAY "NC279A-TEST-3 PASS"
           ELSE
               DISPLAY "NC279A-TEST-3 FAIL"
               DISPLAY "  Expected DEFAULT, got >"
                   WS-RESULT "<"
           END-IF.
