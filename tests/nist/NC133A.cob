       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC133A.
      *
      * NIST CCVS-style test: OCCURS DEPENDING ON
      * Tests variable-length table with OCCURS DEPENDING ON
      * and subscript access to variable-length items.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE-SIZE   PIC 9(2) VALUE 5.
       01 WS-TABLE-GROUP.
           05 WS-ITEM     PIC 9(4)
               OCCURS 1 TO 10 TIMES
               DEPENDING ON WS-TABLE-SIZE.
       01 WS-I            PIC 9(2) VALUE ZEROS.
       01 WS-SUM          PIC 9(8) VALUE ZEROS.
       01 WS-EXPECTED     PIC 9(8) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Variable-length table with ODO
      *   Set size to 5, fill with values, verify count
           MOVE 5 TO WS-TABLE-SIZE.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-TABLE-SIZE
               MOVE WS-I TO WS-ITEM(WS-I)
           END-PERFORM.
           IF WS-ITEM(1) = 1
             AND WS-ITEM(3) = 3
             AND WS-ITEM(5) = 5
               DISPLAY "NC133A-TEST-1 PASS"
           ELSE
               DISPLAY "NC133A-TEST-1 FAIL"
               DISPLAY "  ITEM(1)=" WS-ITEM(1)
               DISPLAY "  ITEM(3)=" WS-ITEM(3)
               DISPLAY "  ITEM(5)=" WS-ITEM(5)
           END-IF.
      * Test 2: Subscript access and summation
      *   Sum elements 1..5 = 15, then resize to 3
      *   and sum 1..3 = 6
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-TABLE-SIZE
               ADD WS-ITEM(WS-I) TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 15
               MOVE 3 TO WS-TABLE-SIZE
               MOVE 0 TO WS-SUM
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-TABLE-SIZE
                   ADD WS-ITEM(WS-I) TO WS-SUM
               END-PERFORM
               IF WS-SUM = 6
                   DISPLAY "NC133A-TEST-2 PASS"
               ELSE
                   DISPLAY "NC133A-TEST-2 FAIL"
                   DISPLAY "  After resize sum="
                       WS-SUM " expected 6"
               END-IF
           ELSE
               DISPLAY "NC133A-TEST-2 FAIL"
               DISPLAY "  Initial sum="
                   WS-SUM " expected 15"
           END-IF.
           STOP RUN.
