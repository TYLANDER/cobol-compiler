       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC384A.
      *
      * NIST CCVS-style test: UNSTRING with multiple delimiters
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE          PIC X(30) VALUE SPACES.
       01 WS-FIELD1          PIC X(10) VALUE SPACES.
       01 WS-FIELD2          PIC X(10) VALUE SPACES.
       01 WS-FIELD3          PIC X(10) VALUE SPACES.
       01 WS-CNT1            PIC 99    VALUE 0.
       01 WS-CNT2            PIC 99    VALUE 0.
       01 WS-PTR             PIC 99    VALUE 1.
       PROCEDURE DIVISION.
      * Test 1: UNSTRING with comma delimiter
           MOVE "ABC,DEF,GHI" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1.
           MOVE SPACES TO WS-FIELD2.
           MOVE SPACES TO WS-FIELD3.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           IF WS-FIELD1 = "ABC       "
               AND WS-FIELD2 = "DEF       "
               AND WS-FIELD3 = "GHI       "
               DISPLAY "NC384A-TEST-1 PASS"
           ELSE
               DISPLAY "NC384A-TEST-1 FAIL"
               DISPLAY "  F1=[" WS-FIELD1 "]"
               DISPLAY "  F2=[" WS-FIELD2 "]"
               DISPLAY "  F3=[" WS-FIELD3 "]"
           END-IF.
      * Test 2: UNSTRING with OR delimiter
           MOVE "ABC-DEF,GHI" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1.
           MOVE SPACES TO WS-FIELD2.
           MOVE SPACES TO WS-FIELD3.
           UNSTRING WS-SOURCE DELIMITED BY "," OR "-"
               INTO WS-FIELD1 WS-FIELD2 WS-FIELD3.
           IF WS-FIELD1 = "ABC       "
               AND WS-FIELD2 = "DEF       "
               AND WS-FIELD3 = "GHI       "
               DISPLAY "NC384A-TEST-2 PASS"
           ELSE
               DISPLAY "NC384A-TEST-2 FAIL"
               DISPLAY "  F1=[" WS-FIELD1 "]"
               DISPLAY "  F2=[" WS-FIELD2 "]"
               DISPLAY "  F3=[" WS-FIELD3 "]"
           END-IF.
      * Test 3: UNSTRING with COUNT IN on delimited fields
      * Use COUNT on first two fields which are delimited by ","
           MOVE "AB,CDEF,GHI" TO WS-SOURCE.
           MOVE SPACES TO WS-FIELD1.
           MOVE SPACES TO WS-FIELD2.
           MOVE SPACES TO WS-FIELD3.
           MOVE 0 TO WS-CNT1.
           MOVE 0 TO WS-CNT2.
           UNSTRING WS-SOURCE DELIMITED BY ","
               INTO WS-FIELD1 COUNT IN WS-CNT1
                    WS-FIELD2 COUNT IN WS-CNT2
                    WS-FIELD3.
           IF WS-CNT1 = 2 AND WS-CNT2 = 4
               DISPLAY "NC384A-TEST-3 PASS"
           ELSE
               DISPLAY "NC384A-TEST-3 FAIL"
               DISPLAY "  C1=" WS-CNT1 " C2=" WS-CNT2
           END-IF.
           STOP RUN.
