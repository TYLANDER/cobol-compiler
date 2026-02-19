       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC110A.
      *
      * NIST CCVS-style test: PERFORM VARYING
      * Tests PERFORM VARYING with simple loop, accumulating
      * a sum, and PERFORM paragraph UNTIL condition.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I        PIC 9(4) VALUE ZEROS.
       01 WS-SUM      PIC 9(8) VALUE ZEROS.
       01 WS-COUNT    PIC 9(4) VALUE ZEROS.
       01 WS-DONE     PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: PERFORM VARYING simple loop (1 to 5)
           MOVE 0 TO WS-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 5
               ADD 1 TO WS-COUNT
           END-PERFORM.
           IF WS-COUNT = 5
               DISPLAY "NC110A-TEST-1 PASS"
           ELSE
               DISPLAY "NC110A-TEST-1 FAIL"
               DISPLAY "  Expected 5, got " WS-COUNT
           END-IF.
      * Test 2: PERFORM VARYING accumulating a sum (1+2+...+10=55)
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 10
               ADD WS-I TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 55
               DISPLAY "NC110A-TEST-2 PASS"
           ELSE
               DISPLAY "NC110A-TEST-2 FAIL"
               DISPLAY "  Expected 55, got " WS-SUM
           END-IF.
      * Test 3: PERFORM paragraph UNTIL condition
           MOVE 0 TO WS-SUM.
           MOVE 0 TO WS-I.
           PERFORM PARA-ADD-TEN UNTIL WS-I = 5.
           IF WS-SUM = 50
               DISPLAY "NC110A-TEST-3 PASS"
           ELSE
               DISPLAY "NC110A-TEST-3 FAIL"
               DISPLAY "  Expected 50, got " WS-SUM
           END-IF.
           STOP RUN.
       PARA-ADD-TEN.
           ADD 10 TO WS-SUM.
           ADD 1 TO WS-I.
