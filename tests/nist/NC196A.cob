       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC196A.
      *
      * NIST CCVS-style test: Nested IF with compound conditions
      * Tests deeply nested IF statements (3+ levels) with
      * multiple AND/OR conditions at each level.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A          PIC 9(4) VALUE ZEROS.
       01 WS-B          PIC 9(4) VALUE ZEROS.
       01 WS-C          PIC 9(4) VALUE ZEROS.
       01 WS-D          PIC 9(4) VALUE ZEROS.
       01 WS-SUM1       PIC 9(4) VALUE ZEROS.
       01 WS-SUM2       PIC 9(4) VALUE ZEROS.
       01 WS-RESULT     PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Three-level nested IF with AND conditions
      *   A=10, B=20, C=30, D=40
      *   Level 1: IF A < B
      *     Level 2: IF B < C AND C < D
      *       Level 3: IF SUM1 = 30 AND SUM2 = 70
      *         => PASS
           MOVE 10 TO WS-A.
           MOVE 20 TO WS-B.
           MOVE 30 TO WS-C.
           MOVE 40 TO WS-D.
           ADD WS-A WS-B GIVING WS-SUM1.
           ADD WS-C WS-D GIVING WS-SUM2.
           MOVE "FAIL" TO WS-RESULT.
           IF WS-A < WS-B
               IF WS-B < WS-C AND WS-C < WS-D
                   IF WS-SUM1 = 30
                       AND WS-SUM2 = 70
                       MOVE "PASS" TO WS-RESULT
                   END-IF
               END-IF
           END-IF.
           IF WS-RESULT = "PASS"
               DISPLAY "NC196A-TEST-1 PASS"
           ELSE
               DISPLAY "NC196A-TEST-1 FAIL"
               DISPLAY "  Expected PASS, got >" WS-RESULT "<"
           END-IF.
      * Test 2: Nested IF with OR and AND combined
      *   A=5, B=10, C=15, D=20
      *   IF A = 5 OR B = 99
      *     IF B > A AND C > B
      *       IF D = 20 OR D = 99
      *         => PASS
           MOVE 5 TO WS-A.
           MOVE 10 TO WS-B.
           MOVE 15 TO WS-C.
           MOVE 20 TO WS-D.
           MOVE "FAIL" TO WS-RESULT.
           IF WS-A = 5 OR WS-B = 99
               IF WS-B > WS-A AND WS-C > WS-B
                   IF WS-D = 20 OR WS-D = 99
                       MOVE "PASS" TO WS-RESULT
                   END-IF
               END-IF
           END-IF.
           IF WS-RESULT = "PASS"
               DISPLAY "NC196A-TEST-2 PASS"
           ELSE
               DISPLAY "NC196A-TEST-2 FAIL"
               DISPLAY "  Expected PASS, got >" WS-RESULT "<"
           END-IF.
      * Test 3: Nested IF where middle level is FALSE (skips inner)
      *   A=100, B=50, C=25, D=10
      *   IF A > B            => TRUE (100 > 50)
      *     IF B < C AND C < D => FALSE (50 < 25 is false)
      *       MOVE "INNER"    => should NOT execute
      *     ELSE
      *       MOVE "MIDDLE"   => should execute
           MOVE 100 TO WS-A.
           MOVE 50 TO WS-B.
           MOVE 25 TO WS-C.
           MOVE 10 TO WS-D.
           MOVE SPACES TO WS-RESULT.
           IF WS-A > WS-B
               IF WS-B < WS-C AND WS-C < WS-D
                   MOVE "INNER" TO WS-RESULT
               ELSE
                   MOVE "MIDDLE" TO WS-RESULT
               END-IF
           ELSE
               MOVE "OUTER" TO WS-RESULT
           END-IF.
           IF WS-RESULT(1:6) = "MIDDLE"
               DISPLAY "NC196A-TEST-3 PASS"
           ELSE
               DISPLAY "NC196A-TEST-3 FAIL"
               DISPLAY "  Expected MIDDLE, got >" WS-RESULT "<"
           END-IF.
           STOP RUN.
