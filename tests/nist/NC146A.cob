       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC146A.
      *
      * NIST CCVS-style test: EVALUATE TRUE with complex conditions
      * Tests the EVALUATE TRUE statement with WHEN clauses
      * containing compound and relational conditions.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SCORE        PIC 9(3)  VALUE ZEROS.
       01 WS-GRADE        PIC X(1)  VALUE SPACE.
       01 WS-X            PIC 9(4)  VALUE ZEROS.
       01 WS-Y            PIC 9(4)  VALUE ZEROS.
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE TRUE — map score to letter grade
      *   Score = 85 => "B"
           MOVE 85 TO WS-SCORE.
           MOVE SPACE TO WS-GRADE.
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
               DISPLAY "NC146A-TEST-1 PASS"
           ELSE
               DISPLAY "NC146A-TEST-1 FAIL"
               DISPLAY "  Expected B, got >" WS-GRADE "<"
           END-IF.
      * Test 2: EVALUATE TRUE — fall through to OTHER
      *   Score = 50 => "F"
           MOVE 50 TO WS-SCORE.
           MOVE SPACE TO WS-GRADE.
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
               DISPLAY "NC146A-TEST-2 PASS"
           ELSE
               DISPLAY "NC146A-TEST-2 FAIL"
               DISPLAY "  Expected F, got >" WS-GRADE "<"
           END-IF.
      * Test 3: EVALUATE TRUE with compound conditions
      *   X=5, Y=10 => X < 10 AND Y >= 10 => "BOTH"
           MOVE 5 TO WS-X.
           MOVE 10 TO WS-Y.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-X < 10 AND WS-Y >= 10
                   MOVE "BOTH" TO WS-RESULT
               WHEN WS-X < 10
                   MOVE "XONLY" TO WS-RESULT
               WHEN WS-Y >= 10
                   MOVE "YONLY" TO WS-RESULT
               WHEN OTHER
                   MOVE "NONE" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT(1:4) = "BOTH"
               DISPLAY "NC146A-TEST-3 PASS"
           ELSE
               DISPLAY "NC146A-TEST-3 FAIL"
               DISPLAY "  Expected BOTH, got >" WS-RESULT "<"
           END-IF.
           STOP RUN.
