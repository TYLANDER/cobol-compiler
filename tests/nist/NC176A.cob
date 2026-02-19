       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC176A.
      *
      * NIST CCVS-style test: Complex EVALUATE
      * Tests EVALUATE TRUE/FALSE with multiple WHEN clauses
      * and WHEN OTHER fallthrough.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SCORE         PIC 9(3) VALUE ZEROS.
       01 WS-GRADE         PIC X(1) VALUE SPACE.
       01 WS-FLAG          PIC 9(1) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE TRUE with range checks
      *   Score=85 should give grade "B"
           MOVE 85 TO WS-SCORE.
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
           IF WS-GRADE = "B"
               DISPLAY "NC176A-TEST-1 PASS"
           ELSE
               DISPLAY "NC176A-TEST-1 FAIL"
               DISPLAY "  Expected B got " WS-GRADE
           END-IF.
      * Test 2: EVALUATE TRUE hitting WHEN OTHER
      *   Score=50 should give grade "F"
           MOVE 50 TO WS-SCORE.
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
           IF WS-GRADE = "F"
               DISPLAY "NC176A-TEST-2 PASS"
           ELSE
               DISPLAY "NC176A-TEST-2 FAIL"
               DISPLAY "  Expected F got " WS-GRADE
           END-IF.
      * Test 3: EVALUATE FALSE
      *   When FLAG=0, FALSE evaluates the negation
      *   WHEN WS-FLAG = 1 means "when NOT (FLAG=1)"
      *   i.e. when FLAG != 1 => TRUE, so this WHEN fires
           MOVE 0 TO WS-FLAG.
           MOVE SPACE TO WS-GRADE.
           EVALUATE FALSE
               WHEN WS-FLAG = 1
                   MOVE "Y" TO WS-GRADE
               WHEN OTHER
                   MOVE "N" TO WS-GRADE
           END-EVALUATE.
           IF WS-GRADE = "Y"
               DISPLAY "NC176A-TEST-3 PASS"
           ELSE
               DISPLAY "NC176A-TEST-3 FAIL"
               DISPLAY "  Expected Y got " WS-GRADE
           END-IF.
           STOP RUN.
