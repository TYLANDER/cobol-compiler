       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC202A.
      *
      * NIST CCVS-style test: CALL with BY CONTENT
      * Tests that data can be passed BY CONTENT to a subprogram.
      * The subprogram reads the content and sets an output flag.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ALPHA  PIC X(10) VALUE "ORIGINAL  ".
       01 WS-NUM    PIC 9(4) VALUE 5555.
       01 WS-FLAG   PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Sub receives alphanumeric via BY CONTENT
           CALL "IC202A-SUB" USING BY CONTENT WS-ALPHA
                                   BY CONTENT WS-NUM
                                   WS-FLAG.
           IF WS-FLAG = 1
               DISPLAY "IC202A-TEST-1 PASS"
           ELSE
               DISPLAY "IC202A-TEST-1 FAIL"
               DISPLAY "  FLAG=" WS-FLAG
           END-IF.
      * Test 2: Caller's alpha value unchanged after call
           IF WS-ALPHA = "ORIGINAL  "
               DISPLAY "IC202A-TEST-2 PASS"
           ELSE
               DISPLAY "IC202A-TEST-2 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA "<"
           END-IF.
      * Test 3: Caller's numeric value unchanged after call
           IF WS-NUM = 5555
               DISPLAY "IC202A-TEST-3 PASS"
           ELSE
               DISPLAY "IC202A-TEST-3 FAIL"
               DISPLAY "  NUM=" WS-NUM
           END-IF.
           STOP RUN.
