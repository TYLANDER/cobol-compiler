       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC207A.
      *
      * NIST CCVS-style test: CALL with 3+ mixed-type parameters
      * Tests passing four parameters of mixed types (alpha and
      * numeric) together in a single CALL statement.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ALPHA  PIC X(10) VALUE SPACES.
       01 WS-NUM    PIC 9(6) VALUE 0.
       01 WS-CODE   PIC X(4) VALUE SPACES.
       01 WS-AMT    PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Alpha parameter set by subprogram
           CALL "IC207A-SUB" USING WS-ALPHA WS-NUM
                                   WS-CODE WS-AMT.
           IF WS-ALPHA = "MIXED-OK  "
               DISPLAY "IC207A-TEST-1 PASS"
           ELSE
               DISPLAY "IC207A-TEST-1 FAIL"
               DISPLAY "  ALPHA=>" WS-ALPHA "<"
           END-IF.
      * Test 2: Numeric parameter set by subprogram
           IF WS-NUM = 123456
               DISPLAY "IC207A-TEST-2 PASS"
           ELSE
               DISPLAY "IC207A-TEST-2 FAIL"
               DISPLAY "  NUM=" WS-NUM
           END-IF.
      * Test 3: Third and fourth parameters set by subprogram
           IF WS-CODE = "GRPX" AND WS-AMT = 9876
               DISPLAY "IC207A-TEST-3 PASS"
           ELSE
               DISPLAY "IC207A-TEST-3 FAIL"
               DISPLAY "  CODE=>" WS-CODE "<"
               DISPLAY "  AMT=" WS-AMT
           END-IF.
           STOP RUN.
