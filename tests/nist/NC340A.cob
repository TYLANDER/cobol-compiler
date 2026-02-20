       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC340A.
      *
      * NIST CCVS-style test: MOVE CORRESPONDING with groups.
      * Tests that MOVE CORRESPONDING copies only fields with
      * matching names between source and target groups.
      * Uses unique field names to avoid duplicate definition.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE.
          05 WS-S-NAME      PIC X(10) VALUE "ALICE     ".
          05 WS-S-AGE       PIC 9(3) VALUE 30.
          05 WS-S-SALARY    PIC 9(6) VALUE 50000.
          05 WS-S-DEPT      PIC X(5) VALUE "ENGIN".
       01 WS-TARGET.
          05 WS-S-NAME      PIC X(10) VALUE SPACES.
          05 WS-S-AGE       PIC 9(3) VALUE 0.
          05 WS-T-LOCATION  PIC X(10) VALUE SPACES.
          05 WS-S-DEPT      PIC X(5) VALUE SPACES.
       01 WS-SRC-FLAT.
          05 WS-F-ALPHA     PIC X(4) VALUE "ABCD".
          05 WS-F-BETA      PIC 9(2) VALUE 99.
          05 WS-F-GAMMA     PIC X(3) VALUE "XYZ".
       01 WS-DST-FLAT.
          05 WS-F-ALPHA     PIC X(4) VALUE SPACES.
          05 WS-F-DELTA     PIC 9(2) VALUE 0.
          05 WS-F-GAMMA     PIC X(3) VALUE SPACES.
       01 WS-SRC-EXTRA.
          05 WS-E-CODE      PIC X(3) VALUE "ABC".
          05 WS-E-VALUE     PIC 9(5) VALUE 12345.
          05 WS-E-FLAG      PIC 9 VALUE 1.
       01 WS-DST-EXTRA.
          05 WS-E-CODE      PIC X(3) VALUE SPACES.
          05 WS-E-VALUE     PIC 9(5) VALUE 0.
          05 WS-E-STATUS    PIC X(2) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: MOVE CORRESPONDING with some matching fields
      *   WS-S-NAME, WS-S-AGE, WS-S-DEPT match.
      *   WS-S-SALARY/WS-T-LOCATION don't match.
           MOVE CORRESPONDING WS-SOURCE TO WS-TARGET.
           IF WS-S-NAME OF WS-TARGET = "ALICE     "
               AND WS-S-AGE OF WS-TARGET = 30
               AND WS-S-DEPT OF WS-TARGET = "ENGIN"
               AND WS-T-LOCATION = SPACES
               DISPLAY "NC340A-TEST-1 PASS"
           ELSE
               DISPLAY "NC340A-TEST-1 FAIL"
               DISPLAY "  NAME='" WS-S-NAME OF WS-TARGET "'"
               DISPLAY "  AGE=" WS-S-AGE OF WS-TARGET
               DISPLAY "  DEPT='" WS-S-DEPT OF WS-TARGET "'"
               DISPLAY "  LOC='" WS-T-LOCATION "'"
           END-IF.
      * Test 2: MOVE CORRESPONDING with partial match
      *   WS-F-ALPHA and WS-F-GAMMA match.
      *   WS-F-BETA/WS-F-DELTA don't match.
           MOVE CORRESPONDING WS-SRC-FLAT TO WS-DST-FLAT.
           IF WS-F-ALPHA OF WS-DST-FLAT = "ABCD"
               AND WS-F-GAMMA OF WS-DST-FLAT = "XYZ"
               AND WS-F-DELTA = 0
               DISPLAY "NC340A-TEST-2 PASS"
           ELSE
               DISPLAY "NC340A-TEST-2 FAIL"
               DISPLAY "  ALPHA='"
                   WS-F-ALPHA OF WS-DST-FLAT "'"
               DISPLAY "  GAMMA='"
                   WS-F-GAMMA OF WS-DST-FLAT "'"
               DISPLAY "  DELTA="
                   WS-F-DELTA
           END-IF.
      * Test 3: MOVE CORRESPONDING another set
      *   WS-E-CODE and WS-E-VALUE match.
      *   WS-E-FLAG/WS-E-STATUS don't match.
           MOVE CORRESPONDING WS-SRC-EXTRA TO WS-DST-EXTRA.
           IF WS-E-CODE OF WS-DST-EXTRA = "ABC"
               AND WS-E-VALUE OF WS-DST-EXTRA = 12345
               AND WS-E-STATUS = SPACES
               DISPLAY "NC340A-TEST-3 PASS"
           ELSE
               DISPLAY "NC340A-TEST-3 FAIL"
               DISPLAY "  CODE='"
                   WS-E-CODE OF WS-DST-EXTRA "'"
               DISPLAY "  VALUE="
                   WS-E-VALUE OF WS-DST-EXTRA
               DISPLAY "  STATUS='"
                   WS-E-STATUS "'"
           END-IF.
           STOP RUN.
