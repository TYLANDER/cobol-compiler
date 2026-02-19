       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC204A.
      *
      * NIST CCVS-style test: PERFORM THRU with multiple paragraphs
      * Tests PERFORM paragraph THRU paragraph for sequential
      * paragraph execution and counting.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER      PIC 9(4) VALUE ZEROS.
       01 WS-PATH         PIC X(20) VALUE SPACES.
       01 WS-PTR          PIC 9(2)  VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PARA.
      * Test 1: PERFORM THRU executes all paragraphs in range
      *   PARA-A, PARA-B, PARA-C each add 1 => counter = 3
           MOVE 0 TO WS-COUNTER.
           PERFORM PARA-A THRU PARA-C.
           IF WS-COUNTER = 3
               DISPLAY "NC204A-TEST-1 PASS"
           ELSE
               DISPLAY "NC204A-TEST-1 FAIL"
               DISPLAY "  Expected 3, got " WS-COUNTER
           END-IF.
      * Test 2: PERFORM THRU verifying execution path
      *   Track which paragraphs execute via STRING
           MOVE SPACES TO WS-PATH.
           MOVE 1 TO WS-PTR.
           PERFORM PARA-D THRU PARA-F.
           IF WS-PATH(1:3) = "DEF"
               DISPLAY "NC204A-TEST-2 PASS"
           ELSE
               DISPLAY "NC204A-TEST-2 FAIL"
               DISPLAY "  Expected DEF, got >" WS-PATH "<"
           END-IF.
      * Test 3: PERFORM single paragraph (no THRU)
           MOVE 0 TO WS-COUNTER.
           PERFORM PARA-B.
           IF WS-COUNTER = 1
               DISPLAY "NC204A-TEST-3 PASS"
           ELSE
               DISPLAY "NC204A-TEST-3 FAIL"
               DISPLAY "  Expected 1, got " WS-COUNTER
           END-IF.
           STOP RUN.
      * Paragraphs for Test 1: straight-through execution
       PARA-A.
           ADD 1 TO WS-COUNTER.
       PARA-B.
           ADD 1 TO WS-COUNTER.
       PARA-C.
           ADD 1 TO WS-COUNTER.
      * Paragraphs for Test 2: track path
       PARA-D.
           STRING "D" DELIMITED BY SIZE
               INTO WS-PATH WITH POINTER WS-PTR.
       PARA-E.
           STRING "E" DELIMITED BY SIZE
               INTO WS-PATH WITH POINTER WS-PTR.
       PARA-F.
           STRING "F" DELIMITED BY SIZE
               INTO WS-PATH WITH POINTER WS-PTR.
