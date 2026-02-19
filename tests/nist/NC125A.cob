       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC125A.
      *
      * NIST CCVS-style test: Reference modification
      * Tests DISPLAY with ref-mod, MOVE into ref-mod target,
      * and IF comparison with ref-mod operand.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIELD     PIC X(10) VALUE "ABCDEFGHIJ".
       01 WS-PART      PIC X(3)  VALUE SPACES.
       01 WS-CHECK     PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: DISPLAY WS-FIELD(1:3) -- first 3 characters
      *   Extract first 3 chars into WS-PART and verify
           MOVE WS-FIELD(1:3) TO WS-PART.
           IF WS-PART = "ABC"
               DISPLAY "NC125A-TEST-1 PASS"
           ELSE
               DISPLAY "NC125A-TEST-1 FAIL"
               DISPLAY "  Expected ABC, got >" WS-PART "<"
           END-IF.
      * Test 2: MOVE "XYZ" TO WS-FIELD(4:3) -- replace chars 4-6
      *   WS-FIELD starts as ABCDEFGHIJ, after move should be
      *   ABCXYZGHIJ
           MOVE "ABCDEFGHIJ" TO WS-FIELD.
           MOVE "XYZ" TO WS-FIELD(4:3).
           IF WS-FIELD = "ABCXYZGHIJ"
               DISPLAY "NC125A-TEST-2 PASS"
           ELSE
               DISPLAY "NC125A-TEST-2 FAIL"
               DISPLAY "  Expected ABCXYZGHIJ, got >"
                   WS-FIELD "<"
           END-IF.
      * Test 3: IF WS-FIELD(1:6) = "ABCXYZ" -- comparison with
      *   reference modification (field is still ABCXYZGHIJ)
           IF WS-FIELD(1:6) = "ABCXYZ"
               DISPLAY "NC125A-TEST-3 PASS"
           ELSE
               DISPLAY "NC125A-TEST-3 FAIL"
               DISPLAY "  WS-FIELD(1:6) not ABCXYZ, field >"
                   WS-FIELD "<"
           END-IF.
           STOP RUN.
