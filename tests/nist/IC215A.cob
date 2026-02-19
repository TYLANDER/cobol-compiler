       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC215A.
      *
      * NIST CCVS-style test: CALL with numeric parameters
      * Tests passing PIC 9 fields to a subprogram that performs
      * arithmetic computation and returns results via BY REFERENCE.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRICE  PIC 9(4) VALUE 0.
       01 WS-QTY    PIC 9(4) VALUE 0.
       01 WS-TOTAL  PIC 9(6) VALUE 0.
       01 WS-TAX    PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Sub computes total = price * qty
           MOVE 25 TO WS-PRICE.
           MOVE 10 TO WS-QTY.
           MOVE 0 TO WS-TOTAL.
           MOVE 0 TO WS-TAX.
           CALL "IC215A-SUB" USING WS-PRICE WS-QTY
                                   WS-TOTAL WS-TAX.
           IF WS-TOTAL = 250
               DISPLAY "IC215A-TEST-1 PASS"
           ELSE
               DISPLAY "IC215A-TEST-1 FAIL"
               DISPLAY "  TOTAL=" WS-TOTAL
           END-IF.
      * Test 2: Sub computes tax = total / 10
           IF WS-TAX = 25
               DISPLAY "IC215A-TEST-2 PASS"
           ELSE
               DISPLAY "IC215A-TEST-2 FAIL"
               DISPLAY "  TAX=" WS-TAX
           END-IF.
      * Test 3: Different inputs produce correct results
           MOVE 100 TO WS-PRICE.
           MOVE 5 TO WS-QTY.
           MOVE 0 TO WS-TOTAL.
           MOVE 0 TO WS-TAX.
           CALL "IC215A-SUB" USING WS-PRICE WS-QTY
                                   WS-TOTAL WS-TAX.
           IF WS-TOTAL = 500 AND WS-TAX = 50
               DISPLAY "IC215A-TEST-3 PASS"
           ELSE
               DISPLAY "IC215A-TEST-3 FAIL"
               DISPLAY "  TOTAL=" WS-TOTAL
               DISPLAY "  TAX=" WS-TAX
           END-IF.
           STOP RUN.
