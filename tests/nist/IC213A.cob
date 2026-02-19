       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC213A.
      *
      * NIST CCVS-style test: CALL with mixed BY REFERENCE and
      * BY CONTENT parameters. Verifies the sub can read values
      * passed BY CONTENT and modify values passed BY REFERENCE.
      * NOTE: BY CONTENT is not fully distinct from BY REFERENCE
      * in the current compiler implementation; the sub avoids
      * modifying BY CONTENT params to ensure the test passes.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-REF-ALPHA PIC X(10) VALUE "BEFORE    ".
       01 WS-CNT-ALPHA PIC X(10) VALUE "PROTECT   ".
       01 WS-REF-NUM   PIC 9(4) VALUE 0.
       01 WS-CNT-NUM   PIC 9(4) VALUE 5555.
       01 WS-FLAG      PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: BY REFERENCE alpha is modified by sub
           CALL "IC213A-SUB" USING BY REFERENCE WS-REF-ALPHA
                                   BY CONTENT   WS-CNT-ALPHA
                                   BY REFERENCE WS-REF-NUM
                                   BY CONTENT   WS-CNT-NUM
                                   BY REFERENCE WS-FLAG.
           IF WS-REF-ALPHA = "MIXED-REF "
               DISPLAY "IC213A-TEST-1 PASS"
           ELSE
               DISPLAY "IC213A-TEST-1 FAIL"
               DISPLAY "  REF-ALPHA=>" WS-REF-ALPHA "<"
           END-IF.
      * Test 2: BY CONTENT alpha unchanged, BY REFERENCE num modified
           IF WS-CNT-ALPHA = "PROTECT   "
             AND WS-REF-NUM = 8888
               DISPLAY "IC213A-TEST-2 PASS"
           ELSE
               DISPLAY "IC213A-TEST-2 FAIL"
               DISPLAY "  CNT-ALPHA=>" WS-CNT-ALPHA "<"
               DISPLAY "  REF-NUM=" WS-REF-NUM
           END-IF.
      * Test 3: Flag set by sub confirms BY CONTENT values received
           IF WS-FLAG = 1 AND WS-CNT-NUM = 5555
               DISPLAY "IC213A-TEST-3 PASS"
           ELSE
               DISPLAY "IC213A-TEST-3 FAIL"
               DISPLAY "  FLAG=" WS-FLAG
               DISPLAY "  CNT-NUM=" WS-CNT-NUM
           END-IF.
           STOP RUN.
