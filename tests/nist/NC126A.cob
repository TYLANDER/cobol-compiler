       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC126A.
      *
      * NIST CCVS-style test: REDEFINES clause
      * Tests defining a group with REDEFINES, shared memory
      * reads through both views, and writes through REDEFINES.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ORIG-NUM  PIC 9(4) VALUE 1234.
       01 WS-REDEF-ALPHA REDEFINES WS-ORIG-NUM PIC X(4).
       01 WS-GROUP.
           05 WS-PART-A PIC X(3) VALUE "ABC".
           05 WS-PART-B PIC X(3) VALUE "DEF".
       01 WS-GROUP-REDEF REDEFINES WS-GROUP PIC X(6).
       01 WS-CHECK     PIC X(6) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Define numeric field, REDEFINES as alpha, verify
      *   WS-ORIG-NUM is 1234, reading through WS-REDEF-ALPHA
      *   should give "1234" (the character representation)
           MOVE 1234 TO WS-ORIG-NUM.
           IF WS-REDEF-ALPHA = "1234"
               DISPLAY "NC126A-TEST-1 PASS"
           ELSE
               DISPLAY "NC126A-TEST-1 FAIL"
               DISPLAY "  Expected 1234, got >"
                   WS-REDEF-ALPHA "<"
           END-IF.
      * Test 2: MOVE to original group fields, read through
      *   REDEFINES (shared memory). Group has PART-A and PART-B
      *   concatenated, REDEFINES views as single X(6) field
           MOVE "XYZ" TO WS-PART-A.
           MOVE "123" TO WS-PART-B.
           IF WS-GROUP-REDEF = "XYZ123"
               DISPLAY "NC126A-TEST-2 PASS"
           ELSE
               DISPLAY "NC126A-TEST-2 FAIL"
               DISPLAY "  Expected XYZ123, got >"
                   WS-GROUP-REDEF "<"
           END-IF.
      * Test 3: MOVE to REDEFINES field, read through original
      *   Write "PPQQRR" through WS-GROUP-REDEF, then read
      *   WS-PART-A (first 3 bytes) and WS-PART-B (next 3 bytes)
           MOVE "PPQQRR" TO WS-GROUP-REDEF.
           IF WS-PART-A = "PPQ" AND WS-PART-B = "QRR"
               DISPLAY "NC126A-TEST-3 PASS"
           ELSE
               DISPLAY "NC126A-TEST-3 FAIL"
               DISPLAY "  Expected PPQ and QRR, got >"
                   WS-PART-A "< >" WS-PART-B "<"
           END-IF.
           STOP RUN.
