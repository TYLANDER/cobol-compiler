       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC369A.
      *
      * NIST CCVS-style test: Nested EVALUATE statements
      * EVALUATE inside EVALUATE with different subjects.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CATEGORY    PIC X VALUE SPACES.
       01 WS-LEVEL       PIC 9 VALUE 0.
       01 WS-OUTPUT       PIC X(15) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Nested EVALUATE, outer="A" inner=1
      * Category A, Level 1 -> "A-BASIC"
           MOVE "A" TO WS-CATEGORY.
           MOVE 1 TO WS-LEVEL.
           EVALUATE WS-CATEGORY
               WHEN "A"
                   EVALUATE WS-LEVEL
                       WHEN 1
                           MOVE "A-BASIC" TO WS-OUTPUT
                       WHEN 2
                           MOVE "A-ADVANCED" TO WS-OUTPUT
                       WHEN OTHER
                           MOVE "A-UNKNOWN" TO WS-OUTPUT
                   END-EVALUATE
               WHEN "B"
                   EVALUATE WS-LEVEL
                       WHEN 1
                           MOVE "B-BASIC" TO WS-OUTPUT
                       WHEN 2
                           MOVE "B-ADVANCED" TO WS-OUTPUT
                       WHEN OTHER
                           MOVE "B-UNKNOWN" TO WS-OUTPUT
                   END-EVALUATE
               WHEN OTHER
                   MOVE "INVALID" TO WS-OUTPUT
           END-EVALUATE.
           IF WS-OUTPUT = "A-BASIC"
               DISPLAY "NC369A-TEST-1 PASS"
           ELSE
               DISPLAY "NC369A-TEST-1 FAIL"
               DISPLAY "  Expected A-BASIC, got [" WS-OUTPUT "]"
           END-IF.
      * Test 2: Nested EVALUATE, outer="B" inner=2
      * Category B, Level 2 -> "B-ADVANCED"
           MOVE "B" TO WS-CATEGORY.
           MOVE 2 TO WS-LEVEL.
           EVALUATE WS-CATEGORY
               WHEN "A"
                   EVALUATE WS-LEVEL
                       WHEN 1
                           MOVE "A-BASIC" TO WS-OUTPUT
                       WHEN 2
                           MOVE "A-ADVANCED" TO WS-OUTPUT
                       WHEN OTHER
                           MOVE "A-UNKNOWN" TO WS-OUTPUT
                   END-EVALUATE
               WHEN "B"
                   EVALUATE WS-LEVEL
                       WHEN 1
                           MOVE "B-BASIC" TO WS-OUTPUT
                       WHEN 2
                           MOVE "B-ADVANCED" TO WS-OUTPUT
                       WHEN OTHER
                           MOVE "B-UNKNOWN" TO WS-OUTPUT
                   END-EVALUATE
               WHEN OTHER
                   MOVE "INVALID" TO WS-OUTPUT
           END-EVALUATE.
           IF WS-OUTPUT = "B-ADVANCED"
               DISPLAY "NC369A-TEST-2 PASS"
           ELSE
               DISPLAY "NC369A-TEST-2 FAIL"
               DISPLAY "  Expected B-ADVANCED, got ["
                   WS-OUTPUT "]"
           END-IF.
      * Test 3: Nested EVALUATE, outer OTHER path
      * Category C, Level 1 -> "INVALID"
           MOVE "C" TO WS-CATEGORY.
           MOVE 1 TO WS-LEVEL.
           EVALUATE WS-CATEGORY
               WHEN "A"
                   EVALUATE WS-LEVEL
                       WHEN 1
                           MOVE "A-BASIC" TO WS-OUTPUT
                       WHEN OTHER
                           MOVE "A-UNKNOWN" TO WS-OUTPUT
                   END-EVALUATE
               WHEN "B"
                   EVALUATE WS-LEVEL
                       WHEN 1
                           MOVE "B-BASIC" TO WS-OUTPUT
                       WHEN OTHER
                           MOVE "B-UNKNOWN" TO WS-OUTPUT
                   END-EVALUATE
               WHEN OTHER
                   MOVE "INVALID" TO WS-OUTPUT
           END-EVALUATE.
           IF WS-OUTPUT = "INVALID"
               DISPLAY "NC369A-TEST-3 PASS"
           ELSE
               DISPLAY "NC369A-TEST-3 FAIL"
               DISPLAY "  Expected INVALID, got [" WS-OUTPUT "]"
           END-IF.
           STOP RUN.
