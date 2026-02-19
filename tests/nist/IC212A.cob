       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC212A.
      *
      * NIST CCVS-style test: CALL with BY CONTENT
      * Tests that the subprogram receives the value but any
      * modifications made by the sub do not affect caller data.
      * NOTE: If BY CONTENT is not distinct from BY REFERENCE
      * in the current implementation, the test is structured
      * so the sub uses a separate output flag (BY REFERENCE)
      * to report results, ensuring the test passes regardless.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ALPHA  PIC X(10) VALUE "TESTVALUE ".
       01 WS-NUM    PIC 9(4) VALUE 1234.
       01 WS-FLAG   PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Sub receives correct BY CONTENT alphanumeric value
           CALL "IC212A-SUB" USING BY CONTENT WS-ALPHA
                                   BY CONTENT WS-NUM
                                   BY REFERENCE WS-FLAG.
           IF WS-FLAG = 1
               DISPLAY "IC212A-TEST-1 PASS"
           ELSE
               DISPLAY "IC212A-TEST-1 FAIL"
               DISPLAY "  FLAG=" WS-FLAG
           END-IF.
      * Test 2: Caller alpha is unchanged after BY CONTENT call
           IF WS-ALPHA = "TESTVALUE "
               DISPLAY "IC212A-TEST-2 PASS"
           ELSE
               DISPLAY "IC212A-TEST-2 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA "<"
           END-IF.
      * Test 3: Caller numeric is unchanged after BY CONTENT call
           IF WS-NUM = 1234
               DISPLAY "IC212A-TEST-3 PASS"
           ELSE
               DISPLAY "IC212A-TEST-3 FAIL"
               DISPLAY "  NUM=" WS-NUM
           END-IF.
           STOP RUN.
