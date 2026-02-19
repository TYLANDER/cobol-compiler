       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC190A.
      *
      * NIST CCVS-style test: REDEFINES with different PIC
      * Tests REDEFINES clause where the same memory is
      * interpreted under different PICTURE definitions.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RAW            PIC X(8) VALUE "12345678".
       01 WS-RAW-NUM REDEFINES WS-RAW PIC 9(8).
       01 WS-GROUP-A.
           05 WS-PART1       PIC X(4) VALUE "ABCD".
           05 WS-PART2       PIC X(4) VALUE "1234".
       01 WS-GROUP-B REDEFINES WS-GROUP-A.
           05 WS-WHOLE        PIC X(8).
       01 WS-NUMERIC         PIC 9(6)  VALUE 98765.
       01 WS-NUM-X REDEFINES WS-NUMERIC PIC X(6).
       01 WS-RESULT          PIC 9(8)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: REDEFINES X(8) as 9(8)
      *   "12345678" interpreted as numeric => 12345678
           IF WS-RAW-NUM = 12345678
               DISPLAY "NC190A-TEST-1 PASS"
           ELSE
               DISPLAY "NC190A-TEST-1 FAIL"
               DISPLAY "  Expected 12345678, got "
                   WS-RAW-NUM
           END-IF.
      * Test 2: REDEFINES group as single field
      *   WS-PART1="ABCD" WS-PART2="1234"
      *   WS-WHOLE should be "ABCD1234"
           IF WS-WHOLE = "ABCD1234"
               DISPLAY "NC190A-TEST-2 PASS"
           ELSE
               DISPLAY "NC190A-TEST-2 FAIL"
               DISPLAY "  Expected 'ABCD1234', got >"
                   WS-WHOLE "<"
           END-IF.
      * Test 3: Numeric reinterpreted as alphanumeric
      *   WS-NUMERIC = 098765 (PIC 9(6) stores as "098765")
      *   WS-NUM-X should be "098765"
           MOVE 98765 TO WS-NUMERIC.
           IF WS-NUM-X = "098765"
               DISPLAY "NC190A-TEST-3 PASS"
           ELSE
               DISPLAY "NC190A-TEST-3 FAIL"
               DISPLAY "  Expected '098765', got >"
                   WS-NUM-X "<"
           END-IF.
           STOP RUN.
