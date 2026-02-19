       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC157A.
      *
      * NIST CCVS-style test: MOVE with group items
      * Tests alphanumeric group-to-group MOVE, verifying
      * that subordinate fields are properly populated and
      * space-padding occurs for shorter source fields.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC-GROUP.
           05 WS-SRC-NAME   PIC X(10) VALUE "ALICE".
           05 WS-SRC-CITY   PIC X(10) VALUE "BOSTON".
           05 WS-SRC-CODE   PIC X(5)  VALUE "12345".
       01 WS-DST-GROUP.
           05 WS-DST-NAME   PIC X(10) VALUE SPACES.
           05 WS-DST-CITY   PIC X(10) VALUE SPACES.
           05 WS-DST-CODE   PIC X(5)  VALUE SPACES.
       01 WS-SHORT-GROUP.
           05 WS-SHORT-FLD  PIC X(10) VALUE "HELLO".
       01 WS-LONG-GROUP.
           05 WS-LONG-F1    PIC X(5) VALUE SPACES.
           05 WS-LONG-F2    PIC X(5) VALUE SPACES.
           05 WS-LONG-F3    PIC X(5) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Group MOVE copies entire group byte-for-byte
      *   Source has NAME="ALICE     ", CITY="BOSTON     ",
      *   CODE="12345"
           MOVE WS-SRC-GROUP TO WS-DST-GROUP.
           IF WS-DST-NAME = "ALICE"
               AND WS-DST-CITY = "BOSTON"
               AND WS-DST-CODE = "12345"
               DISPLAY "NC157A-TEST-1 PASS"
           ELSE
               DISPLAY "NC157A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-DST-NAME "<"
               DISPLAY "  CITY=>" WS-DST-CITY "<"
               DISPLAY "  CODE=>" WS-DST-CODE "<"
           END-IF.
      * Test 2: Group MOVE from shorter to longer group
      *   WS-SHORT-GROUP is 10 bytes, WS-LONG-GROUP is 15 bytes
      *   Shorter source is space-padded on the right
      *   "HELLO     " fills LONG-F1="HELLO" LONG-F2="     "
           MOVE SPACES TO WS-LONG-GROUP.
           MOVE WS-SHORT-GROUP TO WS-LONG-GROUP.
           IF WS-LONG-F1 = "HELLO"
               AND WS-LONG-F2 = "     "
               AND WS-LONG-F3 = "     "
               DISPLAY "NC157A-TEST-2 PASS"
           ELSE
               DISPLAY "NC157A-TEST-2 FAIL"
               DISPLAY "  F1=>" WS-LONG-F1 "<"
               DISPLAY "  F2=>" WS-LONG-F2 "<"
               DISPLAY "  F3=>" WS-LONG-F3 "<"
           END-IF.
      * Test 3: Verify original source is unchanged after group MOVE
           IF WS-SRC-NAME = "ALICE"
               AND WS-SRC-CITY = "BOSTON"
               AND WS-SRC-CODE = "12345"
               DISPLAY "NC157A-TEST-3 PASS"
           ELSE
               DISPLAY "NC157A-TEST-3 FAIL"
               DISPLAY "  Source changed after MOVE"
               DISPLAY "  NAME=>" WS-SRC-NAME "<"
           END-IF.
           STOP RUN.
