       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC375A.
      *
      * NIST CCVS-style test: Multiple WHEN in EVALUATE
      * (stacked WHEN without ALSO).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CODE         PIC X     VALUE SPACES.
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Stacked WHEN "A" WHEN "B" WHEN "C"
           MOVE "B" TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-CODE
               WHEN "A"
               WHEN "B"
               WHEN "C"
                   MOVE "GROUP1" TO WS-RESULT
               WHEN "D"
                   MOVE "GROUP2" TO WS-RESULT
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "GROUP1    "
               DISPLAY "NC375A-TEST-1 PASS"
           ELSE
               DISPLAY "NC375A-TEST-1 FAIL"
               DISPLAY "  Expected GROUP1, got " WS-RESULT
           END-IF.
      * Test 2: Test "D" path
           MOVE "D" TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-CODE
               WHEN "A"
               WHEN "B"
               WHEN "C"
                   MOVE "GROUP1" TO WS-RESULT
               WHEN "D"
               WHEN "E"
                   MOVE "GROUP2" TO WS-RESULT
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "GROUP2    "
               DISPLAY "NC375A-TEST-2 PASS"
           ELSE
               DISPLAY "NC375A-TEST-2 FAIL"
               DISPLAY "  Expected GROUP2, got " WS-RESULT
           END-IF.
      * Test 3: Test OTHER path
           MOVE "Z" TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE WS-CODE
               WHEN "A"
               WHEN "B"
               WHEN "C"
                   MOVE "GROUP1" TO WS-RESULT
               WHEN "D"
               WHEN "E"
                   MOVE "GROUP2" TO WS-RESULT
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "OTHER     "
               DISPLAY "NC375A-TEST-3 PASS"
           ELSE
               DISPLAY "NC375A-TEST-3 FAIL"
               DISPLAY "  Expected OTHER, got " WS-RESULT
           END-IF.
           STOP RUN.
