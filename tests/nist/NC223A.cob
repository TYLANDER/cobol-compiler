       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC223A.
      *
      * NIST CCVS-style test: PERFORM paragraph with multiple
      * DISPLAY in sequence. Tests that PERFORMing a paragraph
      * executes all statements within it and returns control.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER       PIC 9(4) VALUE 0.
       01 WS-TRACE         PIC X(10) VALUE SPACES.
       01 WS-PTR           PIC 9(2) VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PARA.
      * Test 1: PERFORM a paragraph that has 3 ADDs
      *   Each ADD increments counter by 10 => 30 total
           MOVE 0 TO WS-COUNTER.
           PERFORM TRIPLE-ADD-PARA.
           IF WS-COUNTER = 30
               DISPLAY "NC223A-TEST-1 PASS"
           ELSE
               DISPLAY "NC223A-TEST-1 FAIL"
               DISPLAY "  Expected 30, got " WS-COUNTER
           END-IF.
      * Test 2: PERFORM a paragraph that builds a trace string
      *   using three STRING statements => "ACE"
           MOVE SPACES TO WS-TRACE.
           MOVE 1 TO WS-PTR.
           PERFORM TRACE-PARA.
           IF WS-TRACE(1:3) = "ACE"
               DISPLAY "NC223A-TEST-2 PASS"
           ELSE
               DISPLAY "NC223A-TEST-2 FAIL"
               DISPLAY "  Expected ACE, got >" WS-TRACE "<"
           END-IF.
           STOP RUN.
       TRIPLE-ADD-PARA.
           ADD 10 TO WS-COUNTER.
           ADD 10 TO WS-COUNTER.
           ADD 10 TO WS-COUNTER.
       TRACE-PARA.
           STRING "A" DELIMITED BY SIZE
               INTO WS-TRACE WITH POINTER WS-PTR.
           STRING "C" DELIMITED BY SIZE
               INTO WS-TRACE WITH POINTER WS-PTR.
           STRING "E" DELIMITED BY SIZE
               INTO WS-TRACE WITH POINTER WS-PTR.
