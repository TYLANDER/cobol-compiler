       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC162A.
      *
      * NIST CCVS-style test: INITIALIZE with REPLACING
      * Tests INITIALIZE statement to reset group items
      * and INITIALIZE REPLACING to set specific category values.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-REC1.
           05 WS-NAME       PIC X(10) VALUE "DIRTY".
           05 WS-AMOUNT     PIC 9(5)  VALUE 99999.
           05 WS-CODE       PIC X(3)  VALUE "XYZ".
       01 WS-REC2.
           05 WS-LABEL      PIC X(8)  VALUE "OLD".
           05 WS-COUNT      PIC 9(4)  VALUE 1234.
           05 WS-FLAG       PIC X(1)  VALUE "Y".
       01 WS-REC3.
           05 WS-ALPHA      PIC X(5)  VALUE "ABCDE".
           05 WS-NUM        PIC 9(3)  VALUE 789.
       PROCEDURE DIVISION.
      * Test 1: Simple INITIALIZE resets to default
      *   Alphanumeric fields become SPACES
      *   Numeric fields become ZEROS
           INITIALIZE WS-REC1.
           IF WS-NAME = SPACES
               AND WS-AMOUNT = ZEROS
               AND WS-CODE = SPACES
               DISPLAY "NC162A-TEST-1 PASS"
           ELSE
               DISPLAY "NC162A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-NAME "<"
               DISPLAY "  AMOUNT=" WS-AMOUNT
               DISPLAY "  CODE=>" WS-CODE "<"
           END-IF.
      * Test 2: INITIALIZE REPLACING ALPHANUMERIC BY
      *   Alphanumeric fields become the replacement value
      *   Numeric fields should become ZEROS (default)
           INITIALIZE WS-REC2
               REPLACING ALPHANUMERIC DATA BY "***".
           IF WS-LABEL = "***     "
               AND WS-COUNT = ZEROS
               AND WS-FLAG = "*"
               DISPLAY "NC162A-TEST-2 PASS"
           ELSE
               DISPLAY "NC162A-TEST-2 FAIL"
               DISPLAY "  LABEL=>" WS-LABEL "<"
               DISPLAY "  COUNT=" WS-COUNT
               DISPLAY "  FLAG=>" WS-FLAG "<"
           END-IF.
      * Test 3: INITIALIZE REPLACING NUMERIC BY
      *   Numeric fields become the replacement value
      *   Alphanumeric fields should become SPACES (default)
           MOVE "ABCDE" TO WS-ALPHA.
           MOVE 789 TO WS-NUM.
           INITIALIZE WS-REC3
               REPLACING NUMERIC DATA BY 42.
           IF WS-ALPHA = SPACES
               AND WS-NUM = 42
               DISPLAY "NC162A-TEST-3 PASS"
           ELSE
               DISPLAY "NC162A-TEST-3 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA "<"
               DISPLAY "  NUM=" WS-NUM
           END-IF.
           STOP RUN.
