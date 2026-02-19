       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC142A.
      *
      * NIST CCVS-style test: PERFORM THRU with multiple paragraphs
      * Tests PERFORM paragraph-1 THRU paragraph-n to execute
      * a range of paragraphs in sequence.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TRAIL        PIC X(10) VALUE SPACES.
       01 WS-COUNT        PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
      * Test 1: PERFORM PARA-A THRU PARA-C
      *   Should execute PARA-A, PARA-B, PARA-C in order
      *   Concatenating "A", "B", "C" into WS-TRAIL
           MOVE SPACES TO WS-TRAIL.
           PERFORM PARA-A THRU PARA-C.
           IF WS-TRAIL(1:3) = "ABC"
               DISPLAY "NC142A-TEST-1 PASS"
           ELSE
               DISPLAY "NC142A-TEST-1 FAIL"
               DISPLAY "  Expected ABC, got >" WS-TRAIL "<"
           END-IF.
      * Test 2: PERFORM PARA-B THRU PARA-D 3 TIMES
      *   Each pass appends "B", "C", "D" so 3 passes = 9 chars
      *   WS-COUNT should be 3 after PARA-D runs 3 times
           MOVE SPACES TO WS-TRAIL.
           MOVE 0 TO WS-COUNT.
           PERFORM PARA-B THRU PARA-D 3 TIMES.
           IF WS-COUNT = 3
               DISPLAY "NC142A-TEST-2 PASS"
           ELSE
               DISPLAY "NC142A-TEST-2 FAIL"
               DISPLAY "  Expected COUNT=3, got " WS-COUNT
           END-IF.
           STOP RUN.
       PARA-A.
           STRING WS-TRAIL DELIMITED BY SPACES
               "A" DELIMITED BY SIZE
               INTO WS-TRAIL.
       PARA-B.
           STRING WS-TRAIL DELIMITED BY SPACES
               "B" DELIMITED BY SIZE
               INTO WS-TRAIL.
       PARA-C.
           STRING WS-TRAIL DELIMITED BY SPACES
               "C" DELIMITED BY SIZE
               INTO WS-TRAIL.
       PARA-D.
           ADD 1 TO WS-COUNT.
