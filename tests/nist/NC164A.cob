       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC164A.
      *
      * NIST CCVS-style test: STRING with multiple sources and
      * mixed delimiters (DELIMITED BY literal and SIZE).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIRST        PIC X(10) VALUE "JOHN".
       01 WS-SEP          PIC X(1)  VALUE ",".
       01 WS-LAST         PIC X(10) VALUE "DOE".
       01 WS-DEST         PIC X(30) VALUE SPACES.
       01 WS-PTR          PIC 9(4)  VALUE ZEROS.
       01 WS-SRC1         PIC X(10) VALUE "HELLO,WRLD".
       01 WS-SRC2         PIC X(5)  VALUE "12345".
       PROCEDURE DIVISION.
      * Test 1: STRING with DELIMITED BY SPACE and SIZE
      *   "JOHN" delimited by SPACE => "JOHN"
      *   "," delimited by SIZE => ","
      *   "DOE" delimited by SPACE => "DOE"
      *   Result = "JOHN,DOE"
           MOVE SPACES TO WS-DEST.
           STRING WS-FIRST DELIMITED BY SPACE
               WS-SEP DELIMITED BY SIZE
               WS-LAST DELIMITED BY SPACE
               INTO WS-DEST.
           IF WS-DEST(1:8) = "JOHN,DOE"
               DISPLAY "NC164A-TEST-1 PASS"
           ELSE
               DISPLAY "NC164A-TEST-1 FAIL"
               DISPLAY "  Expected JOHN,DOE got >"
                   WS-DEST "<"
           END-IF.
      * Test 2: STRING with DELIMITED BY literal ","
      *   "HELLO,WRLD" delimited by "," => "HELLO"
      *   "12345" delimited by SIZE => "12345"
      *   Result = "HELLO12345"
           MOVE SPACES TO WS-DEST.
           STRING WS-SRC1 DELIMITED BY ","
               WS-SRC2 DELIMITED BY SIZE
               INTO WS-DEST.
           IF WS-DEST(1:10) = "HELLO12345"
               DISPLAY "NC164A-TEST-2 PASS"
           ELSE
               DISPLAY "NC164A-TEST-2 FAIL"
               DISPLAY "  Expected HELLO12345 got >"
                   WS-DEST "<"
           END-IF.
      * Test 3: STRING with POINTER tracking
      *   Build result starting at position 1, then check POINTER
           MOVE SPACES TO WS-DEST.
           MOVE 1 TO WS-PTR.
           STRING "ABC" DELIMITED BY SIZE
               "-" DELIMITED BY SIZE
               "XYZ" DELIMITED BY SIZE
               INTO WS-DEST
               WITH POINTER WS-PTR.
           IF WS-DEST(1:7) = "ABC-XYZ" AND WS-PTR = 8
               DISPLAY "NC164A-TEST-3 PASS"
           ELSE
               DISPLAY "NC164A-TEST-3 FAIL"
               DISPLAY "  Expected ABC-XYZ PTR=8"
               DISPLAY "  Got >" WS-DEST "< PTR=" WS-PTR
           END-IF.
           STOP RUN.
