       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC194A.
      *
      * NIST CCVS-style test: Multiple WHEN clauses in EVALUATE
      * Tests EVALUATE with several WHEN values for the same action,
      * EVALUATE TRUE with compound conditions, and EVALUATE with
      * WHEN OTHER as a catch-all.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CODE       PIC 9(2) VALUE ZEROS.
       01 WS-RESULT     PIC X(10) VALUE SPACES.
       01 WS-SCORE      PIC 9(3) VALUE ZEROS.
       01 WS-GRADE      PIC X(4) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE with multiple WHEN values per action
      *   Codes 1, 2, 3 map to "LOW", 4, 5, 6 map to "MID",
      *   anything else maps to "HIGH"
           MOVE 2 TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-CODE
               WHEN 1
               WHEN 2
               WHEN 3
                   MOVE "LOW" TO WS-RESULT
               WHEN 4
               WHEN 5
               WHEN 6
                   MOVE "MID" TO WS-RESULT
               WHEN OTHER
                   MOVE "HIGH" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT(1:3) = "LOW"
               DISPLAY "NC194A-TEST-1 PASS"
           ELSE
               DISPLAY "NC194A-TEST-1 FAIL"
               DISPLAY "  Expected LOW, got >" WS-RESULT "<"
           END-IF.
      * Test 2: Same EVALUATE, test the second group (code 5 -> MID)
           MOVE 5 TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-CODE
               WHEN 1
               WHEN 2
               WHEN 3
                   MOVE "LOW" TO WS-RESULT
               WHEN 4
               WHEN 5
               WHEN 6
                   MOVE "MID" TO WS-RESULT
               WHEN OTHER
                   MOVE "HIGH" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT(1:3) = "MID"
               DISPLAY "NC194A-TEST-2 PASS"
           ELSE
               DISPLAY "NC194A-TEST-2 FAIL"
               DISPLAY "  Expected MID, got >" WS-RESULT "<"
           END-IF.
      * Test 3: EVALUATE TRUE with range-like conditions
      *   Score 85 should get grade "B"
           MOVE 85 TO WS-SCORE.
           MOVE SPACES TO WS-GRADE.
           EVALUATE TRUE
               WHEN WS-SCORE >= 90
                   MOVE "A" TO WS-GRADE
               WHEN WS-SCORE >= 80
                   MOVE "B" TO WS-GRADE
               WHEN WS-SCORE >= 70
                   MOVE "C" TO WS-GRADE
               WHEN OTHER
                   MOVE "F" TO WS-GRADE
           END-EVALUATE.
           IF WS-GRADE(1:1) = "B"
               DISPLAY "NC194A-TEST-3 PASS"
           ELSE
               DISPLAY "NC194A-TEST-3 FAIL"
               DISPLAY "  Expected B, got >" WS-GRADE "<"
           END-IF.
           STOP RUN.
