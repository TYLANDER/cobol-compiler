       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC138A.
      *
      * NIST CCVS-style test: Complex condition tests
      * Tests compound AND, compound OR, and NOT conditions.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A           PIC 9(4) VALUE ZEROS.
       01 WS-B           PIC 9(4) VALUE ZEROS.
       01 WS-C           PIC 9(4) VALUE ZEROS.
       01 WS-D           PIC 9(4) VALUE ZEROS.
       01 WS-RESULT      PIC X(4) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: IF A > B AND C < D (compound AND)
      *   A=10, B=5, C=3, D=8 => both true => PASS
           MOVE 10 TO WS-A.
           MOVE 5 TO WS-B.
           MOVE 3 TO WS-C.
           MOVE 8 TO WS-D.
           MOVE "FAIL" TO WS-RESULT.
           IF WS-A > WS-B AND WS-C < WS-D
               MOVE "PASS" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "PASS"
               DISPLAY "NC138A-TEST-1 PASS"
           ELSE
               DISPLAY "NC138A-TEST-1 FAIL"
               DISPLAY "  Expected AND true, got FAIL"
           END-IF.
      * Test 2: IF A = B OR C = D (compound OR)
      *   A=5, B=9, C=7, D=7 => first false, second true => PASS
           MOVE 5 TO WS-A.
           MOVE 9 TO WS-B.
           MOVE 7 TO WS-C.
           MOVE 7 TO WS-D.
           MOVE "FAIL" TO WS-RESULT.
           IF WS-A = WS-B OR WS-C = WS-D
               MOVE "PASS" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "PASS"
               DISPLAY "NC138A-TEST-2 PASS"
           ELSE
               DISPLAY "NC138A-TEST-2 FAIL"
               DISPLAY "  Expected OR true, got FAIL"
           END-IF.
      * Test 3: IF NOT (A = B)
      *   A=4, B=7 => A <> B => NOT (A = B) is true => PASS
           MOVE 4 TO WS-A.
           MOVE 7 TO WS-B.
           MOVE "FAIL" TO WS-RESULT.
           IF NOT (WS-A = WS-B)
               MOVE "PASS" TO WS-RESULT
           END-IF.
           IF WS-RESULT = "PASS"
               DISPLAY "NC138A-TEST-3 PASS"
           ELSE
               DISPLAY "NC138A-TEST-3 FAIL"
               DISPLAY "  Expected NOT true, got FAIL"
           END-IF.
           STOP RUN.
