       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC247A.
      *
      * NIST CCVS-style test: PERFORM paragraph THRU paragraph
      * Tests sequential execution of paragraph ranges.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER  PIC 9(4) VALUE ZEROS.
       01 WS-TRAIL    PIC X(20) VALUE SPACES.
       01 WS-POS      PIC 9(4) VALUE 1.
       PROCEDURE DIVISION.
           PERFORM TEST-1.
           PERFORM TEST-2.
           PERFORM TEST-3.
           STOP RUN.
      *
       TEST-1.
      * Test 1: PERFORM STEP-A THRU STEP-C
      *   Executes STEP-A(+1), STEP-B(+10), STEP-C(+1) = 12
           MOVE 0 TO WS-COUNTER.
           PERFORM STEP-A THRU STEP-C.
           IF WS-COUNTER = 12
               DISPLAY "NC247A-TEST-1 PASS"
           ELSE
               DISPLAY "NC247A-TEST-1 FAIL"
               DISPLAY "  Expected 12, got " WS-COUNTER
           END-IF.
      *
       TEST-2.
      * Test 2: PERFORM STEP-B THRU STEP-D
      *   Executes STEP-B(+10), STEP-C(+1), STEP-D(+100) = 111
           MOVE 0 TO WS-COUNTER.
           PERFORM STEP-B THRU STEP-D.
           IF WS-COUNTER = 111
               DISPLAY "NC247A-TEST-2 PASS"
           ELSE
               DISPLAY "NC247A-TEST-2 FAIL"
               DISPLAY "  Expected 111, got " WS-COUNTER
           END-IF.
      *
       TEST-3.
      * Test 3: PERFORM STEP-A THRU STEP-D with 2 TIMES
      *   All four paragraphs twice, counter = 2*(1+10+1+100)=224
           MOVE 0 TO WS-COUNTER.
           PERFORM STEP-A THRU STEP-D 2 TIMES.
           IF WS-COUNTER = 224
               DISPLAY "NC247A-TEST-3 PASS"
           ELSE
               DISPLAY "NC247A-TEST-3 FAIL"
               DISPLAY "  Expected 224, got " WS-COUNTER
           END-IF.
      *
       STEP-A.
           ADD 1 TO WS-COUNTER.
       STEP-B.
           ADD 10 TO WS-COUNTER.
       STEP-C.
           ADD 1 TO WS-COUNTER.
       STEP-D.
           ADD 100 TO WS-COUNTER.
