       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC224A.
      *
      * NIST CCVS-style test: IF with nested EVALUATE inside
      * Tests that EVALUATE TRUE works correctly when nested
      * within an IF/ELSE block.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CODE          PIC 9(2) VALUE 0.
       01 WS-RESULT        PIC X(10) VALUE SPACES.
       01 WS-FLAG          PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: IF true branch containing EVALUATE
      *   Code=15, which is > 10, so IF fires.
      *   Inside IF, EVALUATE: 15 < 20 => RESULT = "LOW"
           MOVE 15 TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           IF WS-CODE > 10
               EVALUATE TRUE
                   WHEN WS-CODE < 20
                       MOVE "LOW" TO WS-RESULT
                   WHEN WS-CODE < 50
                       MOVE "MID" TO WS-RESULT
                   WHEN OTHER
                       MOVE "HIGH" TO WS-RESULT
               END-EVALUATE
           ELSE
               MOVE "SKIP" TO WS-RESULT
           END-IF.
           IF WS-RESULT(1:3) = "LOW"
               DISPLAY "NC224A-TEST-1 PASS"
           ELSE
               DISPLAY "NC224A-TEST-1 FAIL"
               DISPLAY "  Expected LOW, got >" WS-RESULT "<"
           END-IF.
      * Test 2: IF false branch skips EVALUATE
      *   Code=5, which is NOT > 10, so ELSE fires => "SKIP"
           MOVE 5 TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           IF WS-CODE > 10
               EVALUATE TRUE
                   WHEN WS-CODE < 20
                       MOVE "LOW" TO WS-RESULT
                   WHEN OTHER
                       MOVE "HIGH" TO WS-RESULT
               END-EVALUATE
           ELSE
               MOVE "SKIP" TO WS-RESULT
           END-IF.
           IF WS-RESULT(1:4) = "SKIP"
               DISPLAY "NC224A-TEST-2 PASS"
           ELSE
               DISPLAY "NC224A-TEST-2 FAIL"
               DISPLAY "  Expected SKIP, got >" WS-RESULT "<"
           END-IF.
      * Test 3: Nested EVALUATE hitting OTHER inside IF
      *   Code=99, > 10 so IF fires. 99 not < 20, not < 50 => OTHER
           MOVE 99 TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           IF WS-CODE > 10
               EVALUATE TRUE
                   WHEN WS-CODE < 20
                       MOVE "LOW" TO WS-RESULT
                   WHEN WS-CODE < 50
                       MOVE "MID" TO WS-RESULT
                   WHEN OTHER
                       MOVE "HIGH" TO WS-RESULT
               END-EVALUATE
           ELSE
               MOVE "SKIP" TO WS-RESULT
           END-IF.
           IF WS-RESULT(1:4) = "HIGH"
               DISPLAY "NC224A-TEST-3 PASS"
           ELSE
               DISPLAY "NC224A-TEST-3 FAIL"
               DISPLAY "  Expected HIGH, got >" WS-RESULT "<"
           END-IF.
           STOP RUN.
