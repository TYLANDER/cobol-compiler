       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC217A.
      *
      * NIST CCVS-style test: INSPECT CONVERTING
      * Tests INSPECT CONVERTING to translate characters
      * from one set to another within a string.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA1        PIC X(10) VALUE SPACES.
       01 WS-DATA2        PIC X(10) VALUE SPACES.
       01 WS-DATA3        PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: INSPECT CONVERTING single char
      *   Convert all A's to X's in "ABACADAE"
      *   Result should be "XBXCXDXE"
           MOVE "ABACADAE  " TO WS-DATA1.
           INSPECT WS-DATA1
               CONVERTING "A" TO "X".
           IF WS-DATA1(1:8) = "XBXCXDXE"
               DISPLAY "NC217A-TEST-1 PASS"
           ELSE
               DISPLAY "NC217A-TEST-1 FAIL"
               DISPLAY "  DATA1=>" WS-DATA1 "<"
           END-IF.
      * Test 2: INSPECT CONVERTING multiple chars
      *   Convert A->1, B->2, C->3 in "AABBCC"
      *   Result should be "112233"
           MOVE "AABBCC    " TO WS-DATA2.
           INSPECT WS-DATA2
               CONVERTING "ABC" TO "123".
           IF WS-DATA2(1:6) = "112233"
               DISPLAY "NC217A-TEST-2 PASS"
           ELSE
               DISPLAY "NC217A-TEST-2 FAIL"
               DISPLAY "  DATA2=>" WS-DATA2 "<"
           END-IF.
      * Test 3: INSPECT CONVERTING lowercase to uppercase
      *   Convert a->A, b->B, c->C in "aAbBcC"
      *   Result should be "AABBCC"
           MOVE "aAbBcC    " TO WS-DATA3.
           INSPECT WS-DATA3
               CONVERTING "abc" TO "ABC".
           IF WS-DATA3(1:6) = "AABBCC"
               DISPLAY "NC217A-TEST-3 PASS"
           ELSE
               DISPLAY "NC217A-TEST-3 FAIL"
               DISPLAY "  DATA3=>" WS-DATA3 "<"
           END-IF.
           STOP RUN.
