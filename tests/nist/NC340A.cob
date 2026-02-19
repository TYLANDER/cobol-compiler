       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC340A.
      *
      * NIST CCVS-style test: MOVE CORRESPONDING with nested groups.
      * Tests that MOVE CORRESPONDING copies only fields with
      * matching names between source and target groups, even when
      * the groups have different structures.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE.
          05 WS-NAME        PIC X(10) VALUE "ALICE     ".
          05 WS-AGE         PIC 9(3) VALUE 30.
          05 WS-SALARY      PIC 9(6) VALUE 50000.
          05 WS-DEPT        PIC X(5) VALUE "ENGIN".
       01 WS-TARGET.
          05 WS-NAME        PIC X(10) VALUE SPACES.
          05 WS-AGE         PIC 9(3) VALUE 0.
          05 WS-LOCATION    PIC X(10) VALUE SPACES.
          05 WS-DEPT        PIC X(5) VALUE SPACES.
       01 WS-SRC-NESTED.
          05 WS-INNER-A.
             10 WS-FIELD-X  PIC X(5) VALUE "HELLO".
             10 WS-FIELD-Y  PIC 9(3) VALUE 123.
          05 WS-INNER-B.
             10 WS-FIELD-Z  PIC X(5) VALUE "WORLD".
       01 WS-DST-NESTED.
          05 WS-INNER-A.
             10 WS-FIELD-X  PIC X(5) VALUE SPACES.
             10 WS-FIELD-Y  PIC 9(3) VALUE 0.
          05 WS-INNER-C.
             10 WS-FIELD-Z  PIC X(5) VALUE SPACES.
       01 WS-SRC-SIMPLE.
          05 WS-ALPHA       PIC X(4) VALUE "ABCD".
          05 WS-BETA        PIC 9(2) VALUE 99.
          05 WS-GAMMA       PIC X(3) VALUE "XYZ".
       01 WS-DST-SIMPLE.
          05 WS-ALPHA       PIC X(4) VALUE SPACES.
          05 WS-DELTA       PIC 9(2) VALUE 0.
          05 WS-GAMMA       PIC X(3) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: MOVE CORRESPONDING with some matching fields
      *   WS-NAME, WS-AGE, WS-DEPT match. WS-SALARY does not match
      *   WS-LOCATION. WS-LOCATION should remain SPACES.
           MOVE CORRESPONDING WS-SOURCE TO WS-TARGET.
           IF WS-NAME OF WS-TARGET = "ALICE     "
               AND WS-AGE OF WS-TARGET = 30
               AND WS-DEPT OF WS-TARGET = "ENGIN"
               AND WS-LOCATION OF WS-TARGET = SPACES
               DISPLAY "NC340A-TEST-1 PASS"
           ELSE
               DISPLAY "NC340A-TEST-1 FAIL"
               DISPLAY "  NAME='" WS-NAME OF WS-TARGET "'"
               DISPLAY "  AGE=" WS-AGE OF WS-TARGET
               DISPLAY "  DEPT='" WS-DEPT OF WS-TARGET "'"
               DISPLAY "  LOC='" WS-LOCATION OF WS-TARGET "'"
           END-IF.
      * Test 2: MOVE CORRESPONDING with nested groups
      *   WS-INNER-A matches (and its children WS-FIELD-X,
      *   WS-FIELD-Y match). WS-INNER-B/WS-INNER-C don't match
      *   at group level so WS-FIELD-Z should NOT be moved.
           MOVE CORRESPONDING WS-SRC-NESTED TO WS-DST-NESTED.
           IF WS-FIELD-X OF WS-INNER-A OF WS-DST-NESTED = "HELLO"
               AND WS-FIELD-Y OF WS-INNER-A OF WS-DST-NESTED = 123
               AND WS-FIELD-Z OF WS-INNER-C OF WS-DST-NESTED
                   = SPACES
               DISPLAY "NC340A-TEST-2 PASS"
           ELSE
               DISPLAY "NC340A-TEST-2 FAIL"
               DISPLAY "  FIELD-X='"
                   WS-FIELD-X OF WS-INNER-A OF WS-DST-NESTED "'"
               DISPLAY "  FIELD-Y="
                   WS-FIELD-Y OF WS-INNER-A OF WS-DST-NESTED
               DISPLAY "  FIELD-Z='"
                   WS-FIELD-Z OF WS-INNER-C OF WS-DST-NESTED "'"
           END-IF.
      * Test 3: MOVE CORRESPONDING with partial field match
      *   WS-ALPHA and WS-GAMMA match. WS-BETA/WS-DELTA don't match.
      *   WS-DELTA should remain 0.
           MOVE CORRESPONDING WS-SRC-SIMPLE TO WS-DST-SIMPLE.
           IF WS-ALPHA OF WS-DST-SIMPLE = "ABCD"
               AND WS-GAMMA OF WS-DST-SIMPLE = "XYZ"
               AND WS-DELTA OF WS-DST-SIMPLE = 0
               DISPLAY "NC340A-TEST-3 PASS"
           ELSE
               DISPLAY "NC340A-TEST-3 FAIL"
               DISPLAY "  ALPHA='"
                   WS-ALPHA OF WS-DST-SIMPLE "'"
               DISPLAY "  GAMMA='"
                   WS-GAMMA OF WS-DST-SIMPLE "'"
               DISPLAY "  DELTA="
                   WS-DELTA OF WS-DST-SIMPLE
           END-IF.
           STOP RUN.
