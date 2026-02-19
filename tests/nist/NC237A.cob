       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC237A.
      *
      * NIST CCVS-style test: UNSTRING with DELIMITER IN
      * and COUNT IN clauses.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE   PIC X(30) VALUE SPACES.
       01 WS-PART1    PIC X(10) VALUE SPACES.
       01 WS-PART2    PIC X(10) VALUE SPACES.
       01 WS-PART3    PIC X(10) VALUE SPACES.
       01 WS-DELIM1   PIC X(1)  VALUE SPACES.
       01 WS-DELIM2   PIC X(1)  VALUE SPACES.
       01 WS-CNT1     PIC 99    VALUE ZEROS.
       01 WS-CNT2     PIC 99    VALUE ZEROS.
       01 WS-CNT3     PIC 99    VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: UNSTRING with single delimiter
      *   "JOHN,DOE,JR" split by "," => JOHN, DOE, JR
           MOVE "JOHN,DOE,JR" TO WS-SOURCE.
           MOVE SPACES TO WS-PART1.
           MOVE SPACES TO WS-PART2.
           MOVE SPACES TO WS-PART3.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-PART1 WS-PART2 WS-PART3.
           IF WS-PART1 = "JOHN" AND WS-PART2 = "DOE"
               AND WS-PART3 = "JR"
               DISPLAY "NC237A-TEST-1 PASS"
           ELSE
               DISPLAY "NC237A-TEST-1 FAIL"
               DISPLAY "  Got >" WS-PART1 "< >"
                   WS-PART2 "< >" WS-PART3 "<"
           END-IF.
      * Test 2: UNSTRING with DELIMITER IN
      *   "AB-CD" split by "-", delimiter captured
           MOVE "AB-CD" TO WS-SOURCE.
           MOVE SPACES TO WS-PART1.
           MOVE SPACES TO WS-PART2.
           MOVE SPACES TO WS-DELIM1.
           UNSTRING WS-SOURCE DELIMITED BY "-"
               INTO WS-PART1 DELIMITER IN WS-DELIM1
                    WS-PART2.
           IF WS-PART1 = "AB" AND WS-DELIM1 = "-"
               AND WS-PART2 = "CD"
               DISPLAY "NC237A-TEST-2 PASS"
           ELSE
               DISPLAY "NC237A-TEST-2 FAIL"
               DISPLAY "  P1=>" WS-PART1 "< D=>"
                   WS-DELIM1 "< P2=>" WS-PART2 "<"
           END-IF.
      * Test 3: UNSTRING with COUNT IN tracking field lengths
      *   "HELLO,HI" in PIC X(30): after "," the rest is
      *   "HI" + 22 trailing spaces = 24 chars transferred
           MOVE "HELLO,HI" TO WS-SOURCE.
           MOVE SPACES TO WS-PART1.
           MOVE SPACES TO WS-PART2.
           MOVE 0 TO WS-CNT1.
           MOVE 0 TO WS-CNT2.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-PART1 COUNT IN WS-CNT1
                    WS-PART2 COUNT IN WS-CNT2.
           IF WS-CNT1 = 5 AND WS-CNT2 = 24
               DISPLAY "NC237A-TEST-3 PASS"
           ELSE
               DISPLAY "NC237A-TEST-3 FAIL"
               DISPLAY "  Expected C1=5 C2=24, got C1="
                   WS-CNT1 " C2=" WS-CNT2
           END-IF.
           STOP RUN.
