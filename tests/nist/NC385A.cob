       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC385A.
      *
      * NIST CCVS-style test: INSPECT CONVERTING
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA1           PIC X(10) VALUE SPACES.
       01 WS-DATA2           PIC X(10) VALUE SPACES.
       01 WS-DATA3           PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Convert lowercase to uppercase
           MOVE "abcdef" TO WS-DATA1.
           INSPECT WS-DATA1
               CONVERTING "abcdef" TO "ABCDEF".
           IF WS-DATA1 = "ABCDEF    "
               DISPLAY "NC385A-TEST-1 PASS"
           ELSE
               DISPLAY "NC385A-TEST-1 FAIL"
               DISPLAY "  Got [" WS-DATA1 "]"
           END-IF.
      * Test 2: Convert digits to letters
           MOVE "123" TO WS-DATA2.
           INSPECT WS-DATA2
               CONVERTING "123" TO "ABC".
           IF WS-DATA2 = "ABC       "
               DISPLAY "NC385A-TEST-2 PASS"
           ELSE
               DISPLAY "NC385A-TEST-2 FAIL"
               DISPLAY "  Got [" WS-DATA2 "]"
           END-IF.
      * Test 3: Convert with BEFORE INITIAL
           MOVE "AABBAACCAA" TO WS-DATA3.
           INSPECT WS-DATA3
               CONVERTING "A" TO "X"
               BEFORE INITIAL "C".
           IF WS-DATA3 = "XXBBXXCCAA          "
               DISPLAY "NC385A-TEST-3 PASS"
           ELSE
               DISPLAY "NC385A-TEST-3 FAIL"
               DISPLAY "  Got [" WS-DATA3 "]"
           END-IF.
           STOP RUN.
