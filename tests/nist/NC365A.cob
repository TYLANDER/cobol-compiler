       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC365A.
      *
      * NIST CCVS-style test: Large value arithmetic
      * Tests ADD, SUBTRACT, and MULTIPLY with larger values
      * using DISPLAY format fields.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC 9(7) VALUE 1000000.
       01 WS-B PIC 9(7) VALUE 500000.
       01 WS-C PIC 9(7) VALUE 0.
       01 WS-D PIC S9(7) VALUE 0.
       01 WS-E PIC 9(4) VALUE 1234.
       01 WS-F PIC 9(3) VALUE 100.
       01 WS-G PIC 9(7) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: ADD two large values
           ADD WS-A WS-B GIVING WS-C.
           IF WS-C = 1500000
               DISPLAY "NC365A-TEST-1 PASS"
           ELSE
               DISPLAY "NC365A-TEST-1 FAIL"
               DISPLAY "  Expected 1500000, got " WS-C
           END-IF.
      * Test 2: SUBTRACT yielding result
           SUBTRACT WS-B FROM WS-A GIVING WS-C.
           IF WS-C = 500000
               DISPLAY "NC365A-TEST-2 PASS"
           ELSE
               DISPLAY "NC365A-TEST-2 FAIL"
               DISPLAY "  Expected 0500000, got " WS-C
           END-IF.
      * Test 3: MULTIPLY
           MULTIPLY WS-E BY WS-F GIVING WS-G.
           IF WS-G = 123400
               DISPLAY "NC365A-TEST-3 PASS"
           ELSE
               DISPLAY "NC365A-TEST-3 FAIL"
               DISPLAY "  Expected 0123400, got " WS-G
           END-IF.
           STOP RUN.
