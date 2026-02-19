       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC315A.
      *
      * NIST CCVS-style test: Nested IF with EVALUATE inside
      * Tests complex conditional flow using IF statements
      * that contain EVALUATE blocks.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(2) VALUE ZEROS.
       01 WS-B            PIC 9(2) VALUE ZEROS.
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Outer IF true, EVALUATE selects second WHEN
      *   A=15 > 10 so IF is true, B=5 matches WHEN 5
           MOVE 15 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           IF WS-A > 10
               EVALUATE WS-B
                   WHEN 1
                       MOVE "ONE" TO WS-RESULT
                   WHEN 5
                       MOVE "FIVE" TO WS-RESULT
                   WHEN OTHER
                       MOVE "OTHER-B" TO WS-RESULT
               END-EVALUATE
           ELSE
               MOVE "LOW-A" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "FIVE"
               DISPLAY "NC315A-TEST-1 PASS"
           ELSE
               DISPLAY "NC315A-TEST-1 FAIL"
               DISPLAY "  Expected FIVE, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 2: Outer IF false, EVALUATE is skipped
      *   A=3 <= 10 so IF is false, goes to ELSE
           MOVE 3 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           IF WS-A > 10
               EVALUATE WS-B
                   WHEN 5
                       MOVE "FIVE" TO WS-RESULT
                   WHEN OTHER
                       MOVE "OTHER-B" TO WS-RESULT
               END-EVALUATE
           ELSE
               MOVE "LOW-A" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "LOW-A"
               DISPLAY "NC315A-TEST-2 PASS"
           ELSE
               DISPLAY "NC315A-TEST-2 FAIL"
               DISPLAY "  Expected LOW-A, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 3: Outer IF true, EVALUATE falls to OTHER
      *   A=20 > 10 so IF is true, B=99 no match => OTHER
           MOVE 20 TO WS-A.
           MOVE 99 TO WS-B.
           MOVE SPACES TO WS-RESULT.
           IF WS-A > 10
               EVALUATE WS-B
                   WHEN 1
                       MOVE "ONE" TO WS-RESULT
                   WHEN 5
                       MOVE "FIVE" TO WS-RESULT
                   WHEN OTHER
                       MOVE "OTHER-B" TO WS-RESULT
               END-EVALUATE
           ELSE
               MOVE "LOW-A" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "OTHER-B"
               DISPLAY "NC315A-TEST-3 PASS"
           ELSE
               DISPLAY "NC315A-TEST-3 FAIL"
               DISPLAY "  Expected OTHER-B, got >"
                   WS-RESULT "<"
           END-IF.
           STOP RUN.
