       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC119A.
      *
      * NIST CCVS-style test: Complex MOVE operations
      * Tests group-to-group MOVE, numeric decimal alignment,
      * and reference modification.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP-A.
           05 WS-GA-NAME   PIC X(5) VALUE "HELLO".
           05 WS-GA-NUM    PIC 9(3) VALUE 123.
       01 WS-GROUP-B.
           05 WS-GB-NAME   PIC X(5) VALUE SPACES.
           05 WS-GB-NUM    PIC 9(3) VALUE ZEROS.
       01 WS-DEC-SRC       PIC 9(3)V99 VALUE ZEROS.
       01 WS-DEC-DST       PIC 9(5)V99 VALUE ZEROS.
       01 WS-FULLNAME      PIC X(10) VALUE "ABCDEFGHIJ".
       01 WS-PARTIAL       PIC X(5) VALUE SPACES.
       01 WS-REF-RESULT    PIC X(3) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: MOVE group-to-group (alphanumeric transfer)
           MOVE WS-GROUP-A TO WS-GROUP-B.
           IF WS-GB-NAME = "HELLO" AND WS-GB-NUM = 123
               DISPLAY "NC119A-TEST-1 PASS"
           ELSE
               DISPLAY "NC119A-TEST-1 FAIL"
               DISPLAY "  GB-NAME=>" WS-GB-NAME "<"
               DISPLAY "  GB-NUM=" WS-GB-NUM
           END-IF.
      * Test 2: MOVE numeric with decimal alignment
      *   Moving 123.45 into PIC 9(5)V99 should give 00123.45
           MOVE 123.45 TO WS-DEC-SRC.
           MOVE WS-DEC-SRC TO WS-DEC-DST.
           IF WS-DEC-DST = 123.45
               DISPLAY "NC119A-TEST-2 PASS"
           ELSE
               DISPLAY "NC119A-TEST-2 FAIL"
               DISPLAY "  Expected 123.45, got " WS-DEC-DST
           END-IF.
      * Test 3: MOVE with reference modification
           MOVE "ABCDEFGHIJ" TO WS-FULLNAME.
           MOVE WS-FULLNAME(1:3) TO WS-REF-RESULT.
           IF WS-REF-RESULT = "ABC"
               DISPLAY "NC119A-TEST-3 PASS"
           ELSE
               DISPLAY "NC119A-TEST-3 FAIL"
               DISPLAY "  Expected ABC, got >" WS-REF-RESULT "<"
           END-IF.
           STOP RUN.
