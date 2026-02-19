       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC240A.
      *
      * NIST CCVS-style test: Level-88 condition names
      * Tests level-88 evaluation with matching values,
      * non-matching values, and multiple VALUES.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STATUS   PIC X(1) VALUE "A".
           88 STATUS-ACTIVE   VALUE "A".
           88 STATUS-INACTIVE VALUE "I".
       01 WS-COLOR    PIC X(1) VALUE SPACES.
           88 IS-PRIMARY      VALUE "R" "G" "B".
           88 IS-SECONDARY    VALUE "Y" "C" "M".
       PROCEDURE DIVISION.
      * Test 1: Level-88 condition evaluates TRUE when value
      *   matches. WS-STATUS = "A", so STATUS-ACTIVE is true
           MOVE "A" TO WS-STATUS.
           IF STATUS-ACTIVE
               DISPLAY "NC240A-TEST-1 PASS"
           ELSE
               DISPLAY "NC240A-TEST-1 FAIL"
               DISPLAY "  STATUS-ACTIVE should be TRUE"
           END-IF.
      * Test 2: Level-88 condition evaluates FALSE when value
      *   doesn't match. WS-STATUS is "A", STATUS-INACTIVE
      *   expects "I", so it should be false
           IF STATUS-INACTIVE
               DISPLAY "NC240A-TEST-2 FAIL"
               DISPLAY "  STATUS-INACTIVE should be FALSE"
           ELSE
               DISPLAY "NC240A-TEST-2 PASS"
           END-IF.
      * Test 3: Level-88 with multiple VALUES
      *   IS-PRIMARY is true for "R", "G", or "B"
      *   Test with "G" which should match
           MOVE "G" TO WS-COLOR.
           IF IS-PRIMARY
               DISPLAY "NC240A-TEST-3 PASS"
           ELSE
               DISPLAY "NC240A-TEST-3 FAIL"
               DISPLAY "  IS-PRIMARY should be TRUE for G"
           END-IF.
           STOP RUN.
