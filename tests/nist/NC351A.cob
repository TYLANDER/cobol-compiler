       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC351A.
      *
      * NIST CCVS-style test: INSPECT TALLYING with BEFORE/AFTER
      * INITIAL and multiple tallying clauses.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STRING-1     PIC X(20) VALUE "AABCAADEAA".
       01 WS-STRING-3     PIC X(20) VALUE "ABCABCABCABC".
       01 WS-TALLY        PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: TALLYING ALL "A" BEFORE INITIAL "D"
      * String is "AABCAADEAA" padded with spaces to 20.
      * Before "D" we see: "AABCAA" which has 4 letter A.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-STRING-1 TALLYING WS-TALLY
               FOR ALL "A" BEFORE INITIAL "D".
           IF WS-TALLY = 4
               DISPLAY "NC351A-TEST-1 PASS"
           ELSE
               DISPLAY "NC351A-TEST-1 FAIL"
               DISPLAY "  Expected 4, got " WS-TALLY
           END-IF.
      * Test 2: TALLYING ALL "A" AFTER INITIAL "D"
      * After first "D" we see: "EAA          " => 2 letter A.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-STRING-1 TALLYING WS-TALLY
               FOR ALL "A" AFTER INITIAL "D".
           IF WS-TALLY = 2
               DISPLAY "NC351A-TEST-2 PASS"
           ELSE
               DISPLAY "NC351A-TEST-2 FAIL"
               DISPLAY "  Expected 2, got " WS-TALLY
           END-IF.
      * Test 3: Two separate INSPECT tallying statements
      * String is "ABCABCABCABC" padded to 20.
      * ALL "A" => 4, ALL "BC" => 4, total = 8
           MOVE 0 TO WS-TALLY.
           INSPECT WS-STRING-3 TALLYING WS-TALLY
               FOR ALL "A".
           INSPECT WS-STRING-3 TALLYING WS-TALLY
               FOR ALL "BC".
           IF WS-TALLY = 8
               DISPLAY "NC351A-TEST-3 PASS"
           ELSE
               DISPLAY "NC351A-TEST-3 FAIL"
               DISPLAY "  Expected 8, got " WS-TALLY
           END-IF.
           STOP RUN.
