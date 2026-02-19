       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC307A.
      *
      * NIST CCVS-style test: Multiple MOVE CORRESPONDING in sequence
      * Tests that successive MOVE CORRESPONDING operations are
      * independent and do not interfere with each other.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GRP-A.
           05 WS-NAME     PIC X(10) VALUE "ALICE".
           05 WS-CODE     PIC 9(3)  VALUE 100.
       01 WS-GRP-B.
           05 WS-NAME     PIC X(10) VALUE "BOB".
           05 WS-CODE     PIC 9(3)  VALUE 200.
       01 WS-GRP-C.
           05 WS-NAME     PIC X(10) VALUE SPACES.
           05 WS-CODE     PIC 9(3)  VALUE ZEROS.
           05 WS-EXTRA    PIC X(5)  VALUE "XXXXX".
       PROCEDURE DIVISION.
      * Test 1: MOVE CORRESPONDING from A to C
      *   C gets NAME="ALICE", CODE=100, EXTRA unchanged
           MOVE SPACES TO WS-NAME OF WS-GRP-C.
           MOVE ZEROS TO WS-CODE OF WS-GRP-C.
           MOVE "XXXXX" TO WS-EXTRA.
           MOVE CORRESPONDING WS-GRP-A TO WS-GRP-C.
           IF WS-NAME OF WS-GRP-C = "ALICE"
               AND WS-CODE OF WS-GRP-C = 100
               AND WS-EXTRA = "XXXXX"
               DISPLAY "NC307A-TEST-1 PASS"
           ELSE
               DISPLAY "NC307A-TEST-1 FAIL"
               DISPLAY "  NAME=>"
                   WS-NAME OF WS-GRP-C "<"
               DISPLAY "  CODE="
                   WS-CODE OF WS-GRP-C
               DISPLAY "  EXTRA=>" WS-EXTRA "<"
           END-IF.
      * Test 2: MOVE CORRESPONDING from B to C (overwrites A's data)
      *   C gets NAME="BOB", CODE=200, EXTRA still unchanged
           MOVE CORRESPONDING WS-GRP-B TO WS-GRP-C.
           IF WS-NAME OF WS-GRP-C = "BOB"
               AND WS-CODE OF WS-GRP-C = 200
               AND WS-EXTRA = "XXXXX"
               DISPLAY "NC307A-TEST-2 PASS"
           ELSE
               DISPLAY "NC307A-TEST-2 FAIL"
               DISPLAY "  NAME=>"
                   WS-NAME OF WS-GRP-C "<"
               DISPLAY "  CODE="
                   WS-CODE OF WS-GRP-C
               DISPLAY "  EXTRA=>" WS-EXTRA "<"
           END-IF.
      * Test 3: Verify source groups are unchanged after both MOVEs
      *   A should still be ALICE/100, B should still be BOB/200
           IF WS-NAME OF WS-GRP-A = "ALICE"
               AND WS-CODE OF WS-GRP-A = 100
               AND WS-NAME OF WS-GRP-B = "BOB"
               AND WS-CODE OF WS-GRP-B = 200
               DISPLAY "NC307A-TEST-3 PASS"
           ELSE
               DISPLAY "NC307A-TEST-3 FAIL"
               DISPLAY "  A-NAME=>"
                   WS-NAME OF WS-GRP-A "<"
               DISPLAY "  A-CODE="
                   WS-CODE OF WS-GRP-A
               DISPLAY "  B-NAME=>"
                   WS-NAME OF WS-GRP-B "<"
               DISPLAY "  B-CODE="
                   WS-CODE OF WS-GRP-B
           END-IF.
           STOP RUN.
