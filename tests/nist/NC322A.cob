       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC322A.
      *
      * NIST CCVS-style test: OCCURS with variable subscript
      * Tests table processing with a variable controlling count.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
          05 WS-ELEM        PIC 9(4) OCCURS 10 TIMES.
       01 WS-COUNT          PIC 9(2) VALUE 0.
       01 WS-IDX            PIC 9(2) VALUE 0.
       01 WS-SUM            PIC 9(6) VALUE 0.
       01 WS-EXPECTED       PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Fill 5 elements and sum them
      *   Elements 1-5 get values 10,20,30,40,50. Sum = 150.
           MOVE 5 TO WS-COUNT.
           MOVE 1 TO WS-IDX.
           PERFORM UNTIL WS-IDX > WS-COUNT
               MULTIPLY WS-IDX BY 10 GIVING WS-ELEM(WS-IDX)
               ADD 1 TO WS-IDX
           END-PERFORM.
           MOVE 0 TO WS-SUM.
           MOVE 1 TO WS-IDX.
           PERFORM UNTIL WS-IDX > WS-COUNT
               ADD WS-ELEM(WS-IDX) TO WS-SUM
               ADD 1 TO WS-IDX
           END-PERFORM.
           IF WS-SUM = 150
               DISPLAY "NC322A-TEST-1 PASS"
           ELSE
               DISPLAY "NC322A-TEST-1 FAIL"
               DISPLAY "  Expected 150, got " WS-SUM
           END-IF.
      * Test 2: Fill 3 elements and verify individual access
      *   Element(1)=100, Element(2)=200, Element(3)=300.
           MOVE 100 TO WS-ELEM(1).
           MOVE 200 TO WS-ELEM(2).
           MOVE 300 TO WS-ELEM(3).
           IF WS-ELEM(2) = 200
               DISPLAY "NC322A-TEST-2 PASS"
           ELSE
               DISPLAY "NC322A-TEST-2 FAIL"
               DISPLAY "  Expected ELEM(2)=200, got "
                   WS-ELEM(2)
           END-IF.
      * Test 3: Change count and re-sum only first 3
      *   Elements 1-3 are 100,200,300. Sum = 600.
           MOVE 3 TO WS-COUNT.
           MOVE 0 TO WS-SUM.
           MOVE 1 TO WS-IDX.
           PERFORM UNTIL WS-IDX > WS-COUNT
               ADD WS-ELEM(WS-IDX) TO WS-SUM
               ADD 1 TO WS-IDX
           END-PERFORM.
           IF WS-SUM = 600
               DISPLAY "NC322A-TEST-3 PASS"
           ELSE
               DISPLAY "NC322A-TEST-3 FAIL"
               DISPLAY "  Expected 600, got " WS-SUM
           END-IF.
           STOP RUN.
