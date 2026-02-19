       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC131A.
      *
      * NIST CCVS-style test: Complex EVALUATE statement
      * Tests EVALUATE TRUE, EVALUATE var ALSO var,
      * and EVALUATE with WHEN OTHER.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SCORE        PIC 9(3) VALUE ZEROS.
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       01 WS-X            PIC 9(2) VALUE ZEROS.
       01 WS-Y            PIC 9(2) VALUE ZEROS.
       01 WS-CATEGORY     PIC X(10) VALUE SPACES.
       01 WS-CODE         PIC 9(2) VALUE ZEROS.
       01 WS-DESC         PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE TRUE with multiple WHEN clauses
           MOVE 85 TO WS-SCORE.
           EVALUATE TRUE
               WHEN WS-SCORE >= 90
                   MOVE "EXCELLENT" TO WS-RESULT
               WHEN WS-SCORE >= 80
                   MOVE "GOOD" TO WS-RESULT
               WHEN WS-SCORE >= 70
                   MOVE "AVERAGE" TO WS-RESULT
               WHEN OTHER
                   MOVE "BELOW" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "GOOD"
               DISPLAY "NC131A-TEST-1 PASS"
           ELSE
               DISPLAY "NC131A-TEST-1 FAIL"
               DISPLAY "  Expected GOOD, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 2: EVALUATE variable ALSO variable
           MOVE 1 TO WS-X.
           MOVE 2 TO WS-Y.
           EVALUATE WS-X ALSO WS-Y
               WHEN 1 ALSO 1
                   MOVE "BOTH-ONE" TO WS-CATEGORY
               WHEN 1 ALSO 2
                   MOVE "ONE-TWO" TO WS-CATEGORY
               WHEN 2 ALSO 1
                   MOVE "TWO-ONE" TO WS-CATEGORY
               WHEN OTHER
                   MOVE "OTHER" TO WS-CATEGORY
           END-EVALUATE.
           IF WS-CATEGORY = "ONE-TWO"
               DISPLAY "NC131A-TEST-2 PASS"
           ELSE
               DISPLAY "NC131A-TEST-2 FAIL"
               DISPLAY "  Expected ONE-TWO, got >"
                   WS-CATEGORY "<"
           END-IF.
      * Test 3: EVALUATE with WHEN OTHER as default
           MOVE 99 TO WS-CODE.
           EVALUATE WS-CODE
               WHEN 1
                   MOVE "FIRST" TO WS-DESC
               WHEN 2
                   MOVE "SECOND" TO WS-DESC
               WHEN 3
                   MOVE "THIRD" TO WS-DESC
               WHEN OTHER
                   MOVE "UNKNOWN" TO WS-DESC
           END-EVALUATE.
           IF WS-DESC = "UNKNOWN"
               DISPLAY "NC131A-TEST-3 PASS"
           ELSE
               DISPLAY "NC131A-TEST-3 FAIL"
               DISPLAY "  Expected UNKNOWN, got >"
                   WS-DESC "<"
           END-IF.
           STOP RUN.
