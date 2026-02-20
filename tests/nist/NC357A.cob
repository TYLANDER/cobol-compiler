       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC357A.
      *
      * NIST CCVS-style test: EVALUATE with multiple WHEN values
      * and WHEN OTHER fallthrough.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CODE           PIC X(1) VALUE SPACES.
       01 WS-RESULT         PIC X(10) VALUE SPACES.
       01 WS-NUM            PIC 9(2) VALUE 0.
       01 WS-RESULT2        PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: EVALUATE TRUE with condition-based WHEN
           MOVE "A" TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-CODE = "A"
                   MOVE "ALPHA" TO WS-RESULT
               WHEN WS-CODE = "B"
                   MOVE "BRAVO" TO WS-RESULT
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "ALPHA     "
               DISPLAY "NC357A-TEST-1 PASS"
           ELSE
               DISPLAY "NC357A-TEST-1 FAIL"
               DISPLAY "  Expected ALPHA, got " WS-RESULT
           END-IF.
      * Test 2: EVALUATE TRUE hitting second WHEN
           MOVE "B" TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           EVALUATE TRUE
               WHEN WS-CODE = "A"
                   MOVE "ALPHA" TO WS-RESULT
               WHEN WS-CODE = "B"
                   MOVE "BRAVO" TO WS-RESULT
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT
           END-EVALUATE.
           IF WS-RESULT = "BRAVO     "
               DISPLAY "NC357A-TEST-2 PASS"
           ELSE
               DISPLAY "NC357A-TEST-2 FAIL"
               DISPLAY "  Expected BRAVO, got " WS-RESULT
           END-IF.
      * Test 3: EVALUATE with numeric subject hitting OTHER
           MOVE 50 TO WS-NUM.
           MOVE SPACES TO WS-RESULT2.
           EVALUATE WS-NUM
               WHEN 10
                   MOVE "TEN" TO WS-RESULT2
               WHEN 20
                   MOVE "TWENTY" TO WS-RESULT2
               WHEN OTHER
                   MOVE "OTHER" TO WS-RESULT2
           END-EVALUATE.
           IF WS-RESULT2 = "OTHER     "
               DISPLAY "NC357A-TEST-3 PASS"
           ELSE
               DISPLAY "NC357A-TEST-3 FAIL"
               DISPLAY "  Expected OTHER, got " WS-RESULT2
           END-IF.
           STOP RUN.
