       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC236A.
      *
      * NIST CCVS-style test: STRING with DELIMITED BY SIZE
      * Tests STRING concatenation, POINTER tracking, and
      * ON OVERFLOW detection.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIELD1   PIC X(5)  VALUE "HELLO".
       01 WS-FIELD2   PIC X(5)  VALUE "WORLD".
       01 WS-TARGET   PIC X(12) VALUE SPACES.
       01 WS-PTR      PIC 99    VALUE 1.
       PROCEDURE DIVISION.
      * Test 1: STRING two fields into one with DELIMITED BY
      *   SIZE. "HELLO" + "WORLD" => "HELLOWORLD  "
           MOVE SPACES TO WS-TARGET.
           STRING WS-FIELD1 DELIMITED BY SIZE
                  WS-FIELD2 DELIMITED BY SIZE
               INTO WS-TARGET.
           IF WS-TARGET = "HELLOWORLD  "
               DISPLAY "NC236A-TEST-1 PASS"
           ELSE
               DISPLAY "NC236A-TEST-1 FAIL"
               DISPLAY "  Expected >HELLOWORLD  <, got >"
                   WS-TARGET "<"
           END-IF.
      * Test 2: STRING with POINTER clause tracking position
      *   Start at 1, after STRING pointer should be 11
           MOVE SPACES TO WS-TARGET.
           MOVE 1 TO WS-PTR.
           STRING WS-FIELD1 DELIMITED BY SIZE
                  WS-FIELD2 DELIMITED BY SIZE
               INTO WS-TARGET
               WITH POINTER WS-PTR.
           IF WS-PTR = 11
               DISPLAY "NC236A-TEST-2 PASS"
           ELSE
               DISPLAY "NC236A-TEST-2 FAIL"
               DISPLAY "  Expected PTR=11, got " WS-PTR
           END-IF.
      * Test 3: STRING with ON OVERFLOW detection
      *   Target is 12 chars, try to string 15 chars in
           MOVE SPACES TO WS-TARGET.
           MOVE 1 TO WS-PTR.
           MOVE "HELLO" TO WS-FIELD1.
           MOVE "WORLD" TO WS-FIELD2.
           STRING WS-FIELD1 DELIMITED BY SIZE
                  WS-FIELD2 DELIMITED BY SIZE
                  WS-FIELD1 DELIMITED BY SIZE
               INTO WS-TARGET
               WITH POINTER WS-PTR
               ON OVERFLOW
                   MOVE 99 TO WS-PTR
           END-STRING.
           IF WS-PTR = 99
               DISPLAY "NC236A-TEST-3 PASS"
           ELSE
               DISPLAY "NC236A-TEST-3 FAIL"
               DISPLAY "  Expected overflow, PTR=" WS-PTR
           END-IF.
           STOP RUN.
