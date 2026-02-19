       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC207A.
      *
      * NIST CCVS-style test: Reference modification
      * Tests reference modification on elementary items, group items,
      * and with various start/length combinations.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STRING       PIC X(10) VALUE "ABCDEFGHIJ".
       01 WS-GROUP.
           05 WS-PART1    PIC X(5) VALUE "HELLO".
           05 WS-PART2    PIC X(5) VALUE "WORLD".
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       01 WS-LONG         PIC X(20) VALUE "12345678901234567890".
       PROCEDURE DIVISION.
      * Test 1: Reference modification on elementary item
      *   WS-STRING(3:4) should give "CDEF"
           MOVE SPACES TO WS-RESULT.
           MOVE WS-STRING(3:4) TO WS-RESULT.
           IF WS-RESULT(1:4) = "CDEF"
               DISPLAY "NC207A-TEST-1 PASS"
           ELSE
               DISPLAY "NC207A-TEST-1 FAIL"
               DISPLAY "  Expected CDEF, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 2: Reference modification on group item
      *   WS-GROUP is "HELLOWORLD", (6:5) should give "WORLD"
           MOVE SPACES TO WS-RESULT.
           MOVE WS-GROUP(6:5) TO WS-RESULT.
           IF WS-RESULT(1:5) = "WORLD"
               DISPLAY "NC207A-TEST-2 PASS"
           ELSE
               DISPLAY "NC207A-TEST-2 FAIL"
               DISPLAY "  Expected WORLD, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 3: Reference modification with length > available
      *   WS-LONG(15:6) should give "567890"
           MOVE SPACES TO WS-RESULT.
           MOVE WS-LONG(15:6) TO WS-RESULT.
           IF WS-RESULT(1:6) = "567890"
               DISPLAY "NC207A-TEST-3 PASS"
           ELSE
               DISPLAY "NC207A-TEST-3 FAIL"
               DISPLAY "  Expected 567890, got >"
                   WS-RESULT "<"
           END-IF.
           STOP RUN.
