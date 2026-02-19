       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC130A.
      *
      * NIST CCVS-style test: CORRESPONDING phrase
      * Tests MOVE CORRESPONDING and ADD CORRESPONDING
      * between group items with matching field names.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE-GRP.
           05 WS-FNAME    PIC X(10) VALUE "JOHN".
           05 WS-LNAME    PIC X(10) VALUE "SMITH".
           05 WS-AGE      PIC 9(3)  VALUE 30.
       01 WS-DEST-GRP.
           05 WS-FNAME    PIC X(10) VALUE SPACES.
           05 WS-LNAME    PIC X(10) VALUE SPACES.
           05 WS-AGE      PIC 9(3)  VALUE ZEROS.
           05 WS-EXTRA    PIC X(5)  VALUE "XXXXX".
       01 WS-AMOUNTS-A.
           05 WS-AMT1     PIC 9(4)  VALUE 100.
           05 WS-AMT2     PIC 9(4)  VALUE 200.
           05 WS-AMT3     PIC 9(4)  VALUE 300.
       01 WS-AMOUNTS-B.
           05 WS-AMT1     PIC 9(4)  VALUE 10.
           05 WS-AMT2     PIC 9(4)  VALUE 20.
           05 WS-AMT3     PIC 9(4)  VALUE 30.
       PROCEDURE DIVISION.
      * Test 1: MOVE CORRESPONDING between groups
      *   Matching names are moved, non-matching left alone
           MOVE CORRESPONDING WS-SOURCE-GRP
               TO WS-DEST-GRP.
           IF WS-FNAME OF WS-DEST-GRP = "JOHN"
             AND WS-LNAME OF WS-DEST-GRP = "SMITH"
             AND WS-AGE OF WS-DEST-GRP = 30
             AND WS-EXTRA = "XXXXX"
               DISPLAY "NC130A-TEST-1 PASS"
           ELSE
               DISPLAY "NC130A-TEST-1 FAIL"
               DISPLAY "  FNAME=>"
                   WS-FNAME OF WS-DEST-GRP "<"
               DISPLAY "  LNAME=>"
                   WS-LNAME OF WS-DEST-GRP "<"
               DISPLAY "  AGE="
                   WS-AGE OF WS-DEST-GRP
               DISPLAY "  EXTRA=>" WS-EXTRA "<"
           END-IF.
      * Test 2: ADD CORRESPONDING between groups
      *   Matching numeric fields are added
           ADD CORRESPONDING WS-AMOUNTS-A
               TO WS-AMOUNTS-B.
           IF WS-AMT1 OF WS-AMOUNTS-B = 110
             AND WS-AMT2 OF WS-AMOUNTS-B = 220
             AND WS-AMT3 OF WS-AMOUNTS-B = 330
               DISPLAY "NC130A-TEST-2 PASS"
           ELSE
               DISPLAY "NC130A-TEST-2 FAIL"
               DISPLAY "  AMT1="
                   WS-AMT1 OF WS-AMOUNTS-B
               DISPLAY "  AMT2="
                   WS-AMT2 OF WS-AMOUNTS-B
               DISPLAY "  AMT3="
                   WS-AMT3 OF WS-AMOUNTS-B
           END-IF.
           STOP RUN.
