       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC122A.
      *
      * NIST CCVS-style test: PERFORM THRU and nested PERFORM
      * Tests PERFORM para-a THRU para-c, inline PERFORM UNTIL,
      * and PERFORM TIMES with nested PERFORM VARYING.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER   PIC 9(4) VALUE ZEROS.
       01 WS-I         PIC 9(4) VALUE ZEROS.
       01 WS-J         PIC 9(4) VALUE ZEROS.
       01 WS-TOTAL     PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: PERFORM para-a THRU para-c, verify all three
      *   paragraphs executed (counter should be 3)
           MOVE 0 TO WS-COUNTER.
           PERFORM PARA-A THRU PARA-C.
           IF WS-COUNTER = 3
               DISPLAY "NC122A-TEST-1 PASS"
           ELSE
               DISPLAY "NC122A-TEST-1 FAIL"
               DISPLAY "  Expected 3, got " WS-COUNTER
           END-IF.
      * Test 2: PERFORM inline with UNTIL (count up to 5)
           MOVE 0 TO WS-COUNTER.
           PERFORM UNTIL WS-COUNTER = 5
               ADD 1 TO WS-COUNTER
           END-PERFORM.
           IF WS-COUNTER = 5
               DISPLAY "NC122A-TEST-2 PASS"
           ELSE
               DISPLAY "NC122A-TEST-2 FAIL"
               DISPLAY "  Expected 5, got " WS-COUNTER
           END-IF.
      * Test 3: PERFORM TIMES with nested PERFORM VARYING
      *   Outer loops 2 times, inner varies WS-J from 1 to 3
      *   Each inner iteration adds 1 to WS-TOTAL => 2 * 3 = 6
           MOVE 0 TO WS-TOTAL.
           PERFORM 2 TIMES
               PERFORM VARYING WS-J FROM 1 BY 1
                 UNTIL WS-J > 3
                   ADD 1 TO WS-TOTAL
               END-PERFORM
           END-PERFORM.
           IF WS-TOTAL = 6
               DISPLAY "NC122A-TEST-3 PASS"
           ELSE
               DISPLAY "NC122A-TEST-3 FAIL"
               DISPLAY "  Expected 6, got " WS-TOTAL
           END-IF.
           STOP RUN.
       PARA-A.
           ADD 1 TO WS-COUNTER.
       PARA-B.
           ADD 1 TO WS-COUNTER.
       PARA-C.
           ADD 1 TO WS-COUNTER.
