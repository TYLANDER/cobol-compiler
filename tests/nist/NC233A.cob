       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC233A.
      *
      * NIST CCVS-style test: Nested IF-ELSE with compound
      * conditions using AND/OR operators.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A        PIC 9(4) VALUE ZEROS.
       01 WS-B        PIC 9(4) VALUE ZEROS.
       01 WS-C        PIC 9(4) VALUE ZEROS.
       01 WS-D        PIC 9(4) VALUE ZEROS.
       01 WS-RESULT   PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: IF A > B AND C < D evaluates correctly
      *   A=10, B=5, C=3, D=8 => both true => "PASS"
           MOVE 10 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE 3 TO WS-C.
           MOVE 8 TO WS-D.
           MOVE "FAIL" TO WS-RESULT.
           IF WS-A > WS-B AND WS-C < WS-D
               MOVE "PASS" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "PASS"
               DISPLAY "NC233A-TEST-1 PASS"
           ELSE
               DISPLAY "NC233A-TEST-1 FAIL"
               DISPLAY "  AND condition should be true"
           END-IF.
      * Test 2: IF A = B OR C = D evaluates correctly
      *   A=10, B=99, C=8, D=8 => first false, second true
           MOVE 10 TO WS-A.
           MOVE 99 TO WS-B.
           MOVE 8 TO WS-C.
           MOVE 8 TO WS-D.
           MOVE "FAIL" TO WS-RESULT.
           IF WS-A = WS-B OR WS-C = WS-D
               MOVE "PASS" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "PASS"
               DISPLAY "NC233A-TEST-2 PASS"
           ELSE
               DISPLAY "NC233A-TEST-2 FAIL"
               DISPLAY "  OR condition should be true"
           END-IF.
      * Test 3: Nested IF/ELSE three levels deep
      *   A=10: outer true, B=20: middle true, C=30: inner true
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 30 TO WS-C.
           MOVE "NONE" TO WS-RESULT.
           IF WS-A = 10
               IF WS-B = 20
                   IF WS-C = 30
                       MOVE "DEEP" TO WS-RESULT
                   ELSE
                       MOVE "MID" TO WS-RESULT
                   END-IF
               ELSE
                   MOVE "OUT" TO WS-RESULT
               END-IF
           ELSE
               MOVE "TOP" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "DEEP"
               DISPLAY "NC233A-TEST-3 PASS"
           ELSE
               DISPLAY "NC233A-TEST-3 FAIL"
               DISPLAY "  Expected DEEP, got " WS-RESULT
           END-IF.
           STOP RUN.
