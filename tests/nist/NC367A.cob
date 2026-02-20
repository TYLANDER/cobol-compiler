       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC367A.
      *
      * NIST CCVS-style test: Condition names (88-levels)
      * in EVALUATE using EVALUATE TRUE WHEN cond-name pattern.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STATUS       PIC 9 VALUE 0.
           88 STATUS-ACTIVE   VALUE 1.
           88 STATUS-INACTIVE VALUE 2.
           88 STATUS-PENDING  VALUE 3.
       01 WS-GRADE        PIC X VALUE SPACES.
           88 GRADE-PASS      VALUE "P".
           88 GRADE-FAIL      VALUE "F".
           88 GRADE-DEFER     VALUE "D".
       01 WS-OUTPUT       PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE TRUE with 88-level on numeric
           MOVE 1 TO WS-STATUS.
           EVALUATE TRUE
               WHEN STATUS-ACTIVE
                   MOVE "ACTIVE" TO WS-OUTPUT
               WHEN STATUS-INACTIVE
                   MOVE "INACTIVE" TO WS-OUTPUT
               WHEN STATUS-PENDING
                   MOVE "PENDING" TO WS-OUTPUT
               WHEN OTHER
                   MOVE "UNKNOWN" TO WS-OUTPUT
           END-EVALUATE.
           IF WS-OUTPUT = "ACTIVE"
               DISPLAY "NC367A-TEST-1 PASS"
           ELSE
               DISPLAY "NC367A-TEST-1 FAIL"
               DISPLAY "  Expected ACTIVE, got [" WS-OUTPUT "]"
           END-IF.
      * Test 2: EVALUATE TRUE with 88-level value 3
           MOVE 3 TO WS-STATUS.
           EVALUATE TRUE
               WHEN STATUS-ACTIVE
                   MOVE "ACTIVE" TO WS-OUTPUT
               WHEN STATUS-INACTIVE
                   MOVE "INACTIVE" TO WS-OUTPUT
               WHEN STATUS-PENDING
                   MOVE "PENDING" TO WS-OUTPUT
               WHEN OTHER
                   MOVE "UNKNOWN" TO WS-OUTPUT
           END-EVALUATE.
           IF WS-OUTPUT = "PENDING"
               DISPLAY "NC367A-TEST-2 PASS"
           ELSE
               DISPLAY "NC367A-TEST-2 FAIL"
               DISPLAY "  Expected PENDING, got [" WS-OUTPUT "]"
           END-IF.
      * Test 3: EVALUATE TRUE with 88-level on alpha
           MOVE "F" TO WS-GRADE.
           EVALUATE TRUE
               WHEN GRADE-PASS
                   MOVE "PASSED" TO WS-OUTPUT
               WHEN GRADE-FAIL
                   MOVE "FAILED" TO WS-OUTPUT
               WHEN GRADE-DEFER
                   MOVE "DEFERRED" TO WS-OUTPUT
               WHEN OTHER
                   MOVE "NONE" TO WS-OUTPUT
           END-EVALUATE.
           IF WS-OUTPUT = "FAILED"
               DISPLAY "NC367A-TEST-3 PASS"
           ELSE
               DISPLAY "NC367A-TEST-3 FAIL"
               DISPLAY "  Expected FAILED, got [" WS-OUTPUT "]"
           END-IF.
           STOP RUN.
