       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC135A.
      *
      * NIST CCVS-style test: MULTIPLY statement variations
      * Tests MULTIPLY BY, MULTIPLY BY GIVING, and
      * MULTIPLY with ON SIZE ERROR.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A           PIC 9(4) VALUE ZEROS.
       01 WS-B           PIC 9(4) VALUE ZEROS.
       01 WS-C           PIC 9(4) VALUE ZEROS.
       01 WS-FLAG         PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: MULTIPLY A BY B (B = A * B)
           MOVE 3 TO WS-A.
           MOVE 5 TO WS-B.
           MULTIPLY WS-A BY WS-B.
           IF WS-B = 15
               DISPLAY "NC135A-TEST-1 PASS"
           ELSE
               DISPLAY "NC135A-TEST-1 FAIL"
               DISPLAY "  Expected B=15, got " WS-B
           END-IF.
      * Test 2: MULTIPLY A BY B GIVING C
      *   C = A * B, B is unchanged
           MOVE 7 TO WS-A.
           MOVE 8 TO WS-B.
           MOVE 0 TO WS-C.
           MULTIPLY WS-A BY WS-B GIVING WS-C.
           IF WS-C = 56
               DISPLAY "NC135A-TEST-2 PASS"
           ELSE
               DISPLAY "NC135A-TEST-2 FAIL"
               DISPLAY "  Expected C=56, got " WS-C
           END-IF.
      * Test 3: MULTIPLY with ON SIZE ERROR
      *   Result overflows PIC 9(4) (max 9999)
           MOVE 9999 TO WS-A.
           MOVE 2 TO WS-B.
           MOVE 0 TO WS-C.
           MOVE 0 TO WS-FLAG.
           MULTIPLY WS-A BY WS-B GIVING WS-C
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 0 TO WS-FLAG
           END-MULTIPLY.
           IF WS-FLAG = 1
               DISPLAY "NC135A-TEST-3 PASS"
           ELSE
               DISPLAY "NC135A-TEST-3 FAIL"
               DISPLAY "  Expected SIZE ERROR, flag=" WS-FLAG
           END-IF.
           STOP RUN.
