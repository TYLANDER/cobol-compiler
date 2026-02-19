       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC324A.
      *
      * NIST CCVS-style test: STRING with mixed DELIMITED BY
      * Tests STRING with DELIMITED BY SIZE and DELIMITED BY literal.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIRST        PIC X(10) VALUE "JOHN      ".
       01 WS-SEP           PIC X(1)  VALUE " ".
       01 WS-LAST         PIC X(10) VALUE "DOE       ".
       01 WS-RESULT       PIC X(30) VALUE SPACES.
       01 WS-PTR          PIC 9(2) VALUE 1.
       01 WS-SRC1         PIC X(5) VALUE "ABCDE".
       01 WS-SRC2         PIC X(5) VALUE "FG HI".
       01 WS-SRC3         PIC X(5) VALUE "JKLMN".
       01 WS-OUT          PIC X(20) VALUE SPACES.
       01 WS-PTR2         PIC 9(2) VALUE 1.
       01 WS-TITLE        PIC X(4) VALUE "MR. ".
       01 WS-NAME         PIC X(6) VALUE "SMITH ".
       01 WS-SUFFIX       PIC X(4) VALUE "JR. ".
       01 WS-FULL         PIC X(20) VALUE SPACES.
       01 WS-PTR3         PIC 9(2) VALUE 1.
       PROCEDURE DIVISION.
      * Test 1: STRING two fields delimited by " " with separator
      *   "JOHN" + " " + "DOE" => "JOHN DOE"
           MOVE SPACES TO WS-RESULT.
           MOVE 1 TO WS-PTR.
           STRING WS-FIRST DELIMITED BY " "
                  WS-SEP   DELIMITED BY SIZE
                  WS-LAST  DELIMITED BY " "
                  INTO WS-RESULT
                  WITH POINTER WS-PTR.
           IF WS-RESULT = "JOHN DOE              "
               DISPLAY "NC324A-TEST-1 PASS"
           ELSE
               DISPLAY "NC324A-TEST-1 FAIL"
               DISPLAY "  Expected 'JOHN DOE', got '"
                   WS-RESULT "'"
           END-IF.
      * Test 2: STRING three fields with DELIMITED BY SIZE
      *   "ABCDE" + "FG HI" + "JKLMN" => "ABCDEFG HIJKLMN"
           MOVE SPACES TO WS-OUT.
           MOVE 1 TO WS-PTR2.
           STRING WS-SRC1 DELIMITED BY SIZE
                  WS-SRC2 DELIMITED BY SIZE
                  WS-SRC3 DELIMITED BY SIZE
                  INTO WS-OUT
                  WITH POINTER WS-PTR2.
           IF WS-OUT = "ABCDEFG HIJKLMN     "
               DISPLAY "NC324A-TEST-2 PASS"
           ELSE
               DISPLAY "NC324A-TEST-2 FAIL"
               DISPLAY "  Expected 'ABCDEFG HIJKLMN', got '"
                   WS-OUT "'"
           END-IF.
      * Test 3: STRING with mixed literal and SIZE delimiters
      *   "MR." + " " + "SMITH" + " " + "JR." => "MR. SMITH JR."
           MOVE SPACES TO WS-FULL.
           MOVE 1 TO WS-PTR3.
           STRING WS-TITLE DELIMITED BY " "
                  WS-SEP   DELIMITED BY SIZE
                  WS-NAME  DELIMITED BY " "
                  WS-SEP   DELIMITED BY SIZE
                  WS-SUFFIX DELIMITED BY " "
                  INTO WS-FULL
                  WITH POINTER WS-PTR3.
           IF WS-FULL = "MR. SMITH JR.       "
               DISPLAY "NC324A-TEST-3 PASS"
           ELSE
               DISPLAY "NC324A-TEST-3 FAIL"
               DISPLAY "  Expected 'MR. SMITH JR.', got '"
                   WS-FULL "'"
           END-IF.
           STOP RUN.
