       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC116A.
      *
      * NIST CCVS-style test: PERFORM TIMES with nested PERFORMs
      * Tests PERFORM n TIMES, nested PERFORM loops, and
      * PERFORM VARYING with AFTER clause for 2D iteration.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER   PIC 9(4) VALUE ZEROS.
       01 WS-OUTER     PIC 9(4) VALUE ZEROS.
       01 WS-INNER     PIC 9(4) VALUE ZEROS.
       01 WS-PRODUCT   PIC 9(4) VALUE ZEROS.
       01 WS-I         PIC 9(4) VALUE ZEROS.
       01 WS-J         PIC 9(4) VALUE ZEROS.
       01 WS-SUM       PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: PERFORM para-name 5 TIMES, verify counter = 5
           MOVE 0 TO WS-COUNTER.
           PERFORM PARA-INCREMENT 5 TIMES.
           IF WS-COUNTER = 5
               DISPLAY "NC116A-TEST-1 PASS"
           ELSE
               DISPLAY "NC116A-TEST-1 FAIL"
               DISPLAY "  Expected 5, got " WS-COUNTER
           END-IF.
      * Test 2: Nested PERFORM (outer 3, inner 4), verify product
           MOVE 0 TO WS-PRODUCT.
           MOVE 0 TO WS-OUTER.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM 1 BY 1
                 UNTIL WS-J > 4
                   ADD 1 TO WS-PRODUCT
               END-PERFORM
           END-PERFORM.
           IF WS-PRODUCT = 12
               DISPLAY "NC116A-TEST-2 PASS"
           ELSE
               DISPLAY "NC116A-TEST-2 FAIL"
               DISPLAY "  Expected 12, got " WS-PRODUCT
           END-IF.
      * Test 3: PERFORM VARYING with AFTER clause (2D loop)
      *   Outer: WS-I from 1 by 1 until > 3
      *   Inner: WS-J from 1 by 1 until > 2
      *   Should iterate 3 * 2 = 6 times
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 3
             AFTER WS-J FROM 1 BY 1
             UNTIL WS-J > 2
               ADD 1 TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 6
               DISPLAY "NC116A-TEST-3 PASS"
           ELSE
               DISPLAY "NC116A-TEST-3 FAIL"
               DISPLAY "  Expected 6, got " WS-SUM
           END-IF.
           STOP RUN.
       PARA-INCREMENT.
           ADD 1 TO WS-COUNTER.
