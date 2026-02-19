       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC256A.
      *
      * NIST CCVS-style test: EVALUATE with ALSO clause
      * Tests multi-subject EVALUATE matching.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GENDER    PIC X     VALUE SPACES.
       01 WS-AGE       PIC 9(3)  VALUE ZEROS.
       01 WS-RESULT    PIC X(10) VALUE SPACES.
       01 WS-CODE      PIC 9     VALUE ZEROS.
       01 WS-TYPE      PIC X     VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE A ALSO B with matching WHEN
      *   GENDER="M" AGE=25 => "YOUNG-M"
           MOVE "M" TO WS-GENDER.
           MOVE 25 TO WS-AGE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-GENDER ALSO TRUE
               WHEN "M" ALSO WS-AGE < 30
                   MOVE "YOUNG-M" TO WS-RESULT
               WHEN "M" ALSO WS-AGE >= 30
                   MOVE "OLD-M" TO WS-RESULT
               WHEN "F" ALSO WS-AGE < 30
                   MOVE "YOUNG-F" TO WS-RESULT
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "YOUNG-M"
               DISPLAY "NC256A-TEST-1 PASS"
           ELSE
               DISPLAY "NC256A-TEST-1 FAIL"
               DISPLAY "  Expected YOUNG-M, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 2: EVALUATE TRUE ALSO TRUE
      *   CODE=3 TYPE="A" => "THREE-A"
           MOVE 3 TO WS-CODE.
           MOVE "A" TO WS-TYPE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE ALSO TRUE
               WHEN WS-CODE = 1 ALSO WS-TYPE = "A"
                   MOVE "ONE-A" TO WS-RESULT
               WHEN WS-CODE = 3 ALSO WS-TYPE = "A"
                   MOVE "THREE-A" TO WS-RESULT
               WHEN WS-CODE = 3 ALSO WS-TYPE = "B"
                   MOVE "THREE-B" TO WS-RESULT
               WHEN OTHER
                   MOVE "NONE" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "THREE-A"
               DISPLAY "NC256A-TEST-2 PASS"
           ELSE
               DISPLAY "NC256A-TEST-2 FAIL"
               DISPLAY "  Expected THREE-A, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 3: WHEN OTHER as fallthrough
      *   CODE=9 TYPE="Z" => no match, WHEN OTHER
           MOVE 9 TO WS-CODE.
           MOVE "Z" TO WS-TYPE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE ALSO TRUE
               WHEN WS-CODE = 1 ALSO WS-TYPE = "A"
                   MOVE "MATCH1" TO WS-RESULT
               WHEN WS-CODE = 2 ALSO WS-TYPE = "B"
                   MOVE "MATCH2" TO WS-RESULT
               WHEN OTHER
                   MOVE "DEFAULT" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "DEFAULT"
               DISPLAY "NC256A-TEST-3 PASS"
           ELSE
               DISPLAY "NC256A-TEST-3 FAIL"
               DISPLAY "  Expected DEFAULT, got >"
                   WS-RESULT "<"
           END-IF.
           STOP RUN.
