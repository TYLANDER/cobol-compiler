       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM110A.
      *
      * NIST CCVS-style test: REPLACE with multiple replacements
      * Tests a single REPLACE directive with two replacement pairs.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ALPHA PIC X(10) VALUE SPACES.
       01 WS-NUM PIC 9(4) VALUE 0.
       REPLACE ==TAG-A== BY ==WS-ALPHA==
               ==TAG-N== BY ==WS-NUM==.
       PROCEDURE DIVISION.
      * Test 1: REPLACE with two pairs in single directive
           MOVE "MULTI-REPL" TO TAG-A.
           MOVE 7777 TO TAG-N.
           IF WS-ALPHA = "MULTI-REPL" AND WS-NUM = 7777
               DISPLAY "SM110A-TEST-1 PASS"
           ELSE
               DISPLAY "SM110A-TEST-1 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA "<"
               DISPLAY "  NUM=" WS-NUM
           END-IF.
      * Test 2: Verify both replacements applied again
           MOVE "SECOND    " TO TAG-A.
           MOVE 3333 TO TAG-N.
           IF WS-ALPHA = "SECOND    " AND WS-NUM = 3333
               DISPLAY "SM110A-TEST-2 PASS"
           ELSE
               DISPLAY "SM110A-TEST-2 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA "<"
               DISPLAY "  NUM=" WS-NUM
           END-IF.
       REPLACE OFF.
           STOP RUN.
