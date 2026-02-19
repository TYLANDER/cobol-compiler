       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC201A.
      *
      * NIST CCVS-style test: CALL with BY REFERENCE
      * Tests that a subprogram can modify the caller's data
      * when parameters are passed BY REFERENCE.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VALUE PIC X(10) VALUE "HELLO     ".
       01 WS-NUM   PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: BY REFERENCE alphanumeric - sub modifies caller data
           CALL "IC201A-SUB" USING BY REFERENCE WS-VALUE
                                   BY REFERENCE WS-NUM.
           IF WS-VALUE = "MODIFIED  "
               DISPLAY "IC201A-TEST-1 PASS"
           ELSE
               DISPLAY "IC201A-TEST-1 FAIL"
               DISPLAY "  VALUE=>" WS-VALUE "<"
           END-IF.
      * Test 2: BY REFERENCE numeric - sub modifies caller data
           IF WS-NUM = 1234
               DISPLAY "IC201A-TEST-2 PASS"
           ELSE
               DISPLAY "IC201A-TEST-2 FAIL"
               DISPLAY "  NUM=" WS-NUM
           END-IF.
      * Test 3: Call again after reset - verify fresh modification
           MOVE "RESET     " TO WS-VALUE.
           MOVE 0 TO WS-NUM.
           CALL "IC201A-SUB" USING BY REFERENCE WS-VALUE
                                   BY REFERENCE WS-NUM.
           IF WS-VALUE = "MODIFIED  " AND WS-NUM = 1234
               DISPLAY "IC201A-TEST-3 PASS"
           ELSE
               DISPLAY "IC201A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
