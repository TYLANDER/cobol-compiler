       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC299A.
      *
      * NIST CCVS-style test: Nested PERFORM THRU with inline
      * PERFORM. Tests mixed PERFORM styles: paragraph PERFORM
      * THRU combined with inline PERFORM loops.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER      PIC 9(4) VALUE ZEROS.
       01 WS-OUTER-CT     PIC 9(4) VALUE ZEROS.
       01 WS-INNER-CT     PIC 9(4) VALUE ZEROS.
       01 WS-I            PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       NC299A-CONTROL.
           PERFORM NC299A-TEST-1.
           PERFORM NC299A-TEST-2.
           PERFORM NC299A-TEST-3.
           STOP RUN.
       NC299A-TEST-1.
      * Test 1: PERFORM THRU with inline PERFORM inside
      *   STEP-A adds 10, STEP-B does inline PERFORM 3 TIMES
      *   adding 1 each time, STEP-C adds 100
      *   Total: 10 + 3 + 100 = 113
           MOVE 0 TO WS-COUNTER.
           PERFORM NC299A-STEP-A THRU NC299A-STEP-C.
           IF WS-COUNTER = 113
               DISPLAY "NC299A-TEST-1 PASS"
           ELSE
               DISPLAY "NC299A-TEST-1 FAIL"
               DISPLAY "  Expected 113, got " WS-COUNTER
           END-IF.
       NC299A-TEST-2.
      * Test 2: Inline PERFORM VARYING calling paragraph THRU
      *   Outer inline loop runs 2 times
      *   Each iteration does PERFORM ADD-FIVE THRU ADD-FIVE-EXIT
      *   which adds 5. Total: 2 * 5 = 10
           MOVE 0 TO WS-COUNTER.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 2
               PERFORM NC299A-ADD-FIVE THRU NC299A-ADD-FIVE-EXIT
           END-PERFORM.
           IF WS-COUNTER = 10
               DISPLAY "NC299A-TEST-2 PASS"
           ELSE
               DISPLAY "NC299A-TEST-2 FAIL"
               DISPLAY "  Expected 10, got " WS-COUNTER
           END-IF.
       NC299A-TEST-3.
      * Test 3: Nested inline PERFORMs (no THRU)
      *   Outer 3 times, inner 4 times, add 1 each
      *   Total: 3 * 4 = 12
           MOVE 0 TO WS-COUNTER.
           MOVE 0 TO WS-OUTER-CT.
           PERFORM 3 TIMES
               ADD 1 TO WS-OUTER-CT
               PERFORM 4 TIMES
                   ADD 1 TO WS-COUNTER
               END-PERFORM
           END-PERFORM.
           IF WS-COUNTER = 12 AND WS-OUTER-CT = 3
               DISPLAY "NC299A-TEST-3 PASS"
           ELSE
               DISPLAY "NC299A-TEST-3 FAIL"
               DISPLAY "  Expected CTR=12 OUTER=3"
               DISPLAY "  Got CTR=" WS-COUNTER
                   " OUTER=" WS-OUTER-CT
           END-IF.
      * --- Utility paragraphs ---
       NC299A-STEP-A.
           ADD 10 TO WS-COUNTER.
       NC299A-STEP-B.
           PERFORM 3 TIMES
               ADD 1 TO WS-COUNTER
           END-PERFORM.
       NC299A-STEP-C.
           ADD 100 TO WS-COUNTER.
       NC299A-ADD-FIVE.
           ADD 5 TO WS-COUNTER.
       NC299A-ADD-FIVE-EXIT.
           EXIT.
