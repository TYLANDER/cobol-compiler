       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC267A.
      *
      * NIST CCVS-style test: STRING with multiple delimited
      * fields and POINTER tracking.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIRST          PIC X(10) VALUE SPACES.
       01 WS-MIDDLE         PIC X(10) VALUE SPACES.
       01 WS-LAST           PIC X(10) VALUE SPACES.
       01 WS-RESULT         PIC X(40) VALUE SPACES.
       01 WS-PTR            PIC 9(2)  VALUE 1.
       01 WS-AREA           PIC X(3)  VALUE SPACES.
       01 WS-EXCH           PIC X(3)  VALUE SPACES.
       01 WS-NUMB           PIC X(4)  VALUE SPACES.
       01 WS-PHONE          PIC X(20) VALUE SPACES.
       01 WS-PTR2           PIC 9(2)  VALUE 1.
       01 WS-WORD1          PIC X(10) VALUE SPACES.
       01 WS-WORD2          PIC X(10) VALUE SPACES.
       01 WS-SENTENCE       PIC X(40) VALUE SPACES.
       01 WS-PTR3           PIC 9(2)  VALUE 1.
       PROCEDURE DIVISION.
       NC267A-CONTROL.
           PERFORM NC267A-TEST-1
           PERFORM NC267A-TEST-2
           PERFORM NC267A-TEST-3
           STOP RUN.
       NC267A-TEST-1.
      * STRING three name parts delimited by space with pointer
      *   "JOHN" + " " + "Q" + " " + "SMITH" => "JOHN Q SMITH"
      *   Pointer should advance to 13
           MOVE "JOHN" TO WS-FIRST.
           MOVE "Q" TO WS-MIDDLE.
           MOVE "SMITH" TO WS-LAST.
           MOVE SPACES TO WS-RESULT.
           MOVE 1 TO WS-PTR.
           STRING WS-FIRST DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  WS-MIDDLE DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  WS-LAST DELIMITED BY SPACE
               INTO WS-RESULT
               WITH POINTER WS-PTR.
           IF WS-RESULT(1:12) = "JOHN Q SMITH"
               AND WS-PTR = 13
               DISPLAY "NC267A-TEST-1 PASS"
           ELSE
               DISPLAY "NC267A-TEST-1 FAIL"
               DISPLAY "  RESULT=" WS-RESULT
               DISPLAY "  PTR=" WS-PTR
           END-IF.
       NC267A-TEST-2.
      * STRING building phone number "(555)123-4567"
      *   Pointer should track position correctly
           MOVE "555" TO WS-AREA.
           MOVE "123" TO WS-EXCH.
           MOVE "4567" TO WS-NUMB.
           MOVE SPACES TO WS-PHONE.
           MOVE 1 TO WS-PTR2.
           STRING "(" DELIMITED BY SIZE
                  WS-AREA DELIMITED BY SPACE
                  ")" DELIMITED BY SIZE
                  WS-EXCH DELIMITED BY SPACE
                  "-" DELIMITED BY SIZE
                  WS-NUMB DELIMITED BY SPACE
               INTO WS-PHONE
               WITH POINTER WS-PTR2.
           IF WS-PHONE(1:13) = "(555)123-4567"
               AND WS-PTR2 = 14
               DISPLAY "NC267A-TEST-2 PASS"
           ELSE
               DISPLAY "NC267A-TEST-2 FAIL"
               DISPLAY "  PHONE=" WS-PHONE
               DISPLAY "  PTR2=" WS-PTR2
           END-IF.
       NC267A-TEST-3.
      * STRING with pointer starting at position > 1
      *   Pre-fill result, then append starting at position 6
           MOVE "HELLO" TO WS-WORD1.
           MOVE "WORLD" TO WS-WORD2.
           MOVE SPACES TO WS-SENTENCE.
           MOVE "DEAR " TO WS-SENTENCE(1:5).
           MOVE 6 TO WS-PTR3.
           STRING WS-WORD1 DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  WS-WORD2 DELIMITED BY SPACE
               INTO WS-SENTENCE
               WITH POINTER WS-PTR3.
           IF WS-SENTENCE(1:16) = "DEAR HELLO WORLD"
               DISPLAY "NC267A-TEST-3 PASS"
           ELSE
               DISPLAY "NC267A-TEST-3 FAIL"
               DISPLAY "  SENTENCE=" WS-SENTENCE
               DISPLAY "  PTR3=" WS-PTR3
           END-IF.
