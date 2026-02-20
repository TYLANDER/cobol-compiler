       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC359A.
      *
      * NIST CCVS-style test: Reference modification with
      * literal start and length positions.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE         PIC X(20)
                             VALUE "ABCDEFGHIJKLMNOPQRST".
       01 WS-RESULT          PIC X(5)  VALUE SPACES.
       01 WS-TARGET          PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Reference modification with literal positions
      *   Extract 3 characters starting at position 4.
      *   Should yield "DEF" left-justified in PIC X(5).
           MOVE SPACES TO WS-RESULT.
           MOVE WS-SOURCE(4:3) TO WS-RESULT.
           IF WS-RESULT = "DEF  "
               DISPLAY "NC359A-TEST-1 PASS"
           ELSE
               DISPLAY "NC359A-TEST-1 FAIL"
               DISPLAY "  Expected 'DEF  ', got '"
                       WS-RESULT "'"
           END-IF.
      * Test 2: Reference modification extracting 4 chars
      *   Start at position 7, length 4 => "GHIJ".
           MOVE SPACES TO WS-RESULT.
           MOVE WS-SOURCE(7:4) TO WS-RESULT.
           IF WS-RESULT = "GHIJ "
               DISPLAY "NC359A-TEST-2 PASS"
           ELSE
               DISPLAY "NC359A-TEST-2 FAIL"
               DISPLAY "  Expected 'GHIJ ', got '"
                       WS-RESULT "'"
           END-IF.
      * Test 3: Reference modification writing into substring
      *   Move "XYZ" into WS-TARGET starting at position 3
      *   for length 3. Verify target has spaces then XYZ.
           MOVE SPACES TO WS-TARGET.
           MOVE "XYZ" TO WS-TARGET(3:3).
           IF WS-TARGET(1:5) = "  XYZ"
               DISPLAY "NC359A-TEST-3 PASS"
           ELSE
               DISPLAY "NC359A-TEST-3 FAIL"
               DISPLAY "  Expected '  XYZ', got '"
                       WS-TARGET(1:5) "'"
           END-IF.
           STOP RUN.
