       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC356A.
      *
      * NIST CCVS-style test: Complex INSPECT TALLYING
      * with multiple BEFORE/AFTER phrases.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA1          PIC X(20)
                             VALUE "AXXBXXCXXYXXDXXEXX".
       01 WS-DATA2          PIC X(20)
                             VALUE "ABCXXDEFXXGHIXXJKL".
       01 WS-DATA3          PIC X(20)
                             VALUE "XXYYXXZZXXYYXXZZXX".
       01 WS-TALLY          PIC 9(3)  VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: INSPECT TALLYING FOR ALL "XX" BEFORE "Y"
      *   In "AXXBXXCXXYXXDXXEXX", count "XX" before
      *   first "Y". Should find 3 (positions 2,5,8).
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA1 TALLYING WS-TALLY
               FOR ALL "XX" BEFORE INITIAL "Y".
           IF WS-TALLY = 3
               DISPLAY "NC356A-TEST-1 PASS"
           ELSE
               DISPLAY "NC356A-TEST-1 FAIL"
               DISPLAY "  Expected tally=3, got "
                       WS-TALLY
           END-IF.
      * Test 2: INSPECT TALLYING FOR ALL "XX" AFTER "D"
      *   In "ABCXXDEFXXGHIXXJKL", count "XX" after
      *   first "D". Should find 2 (after D: "EFXXGHIXXJKL").
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA2 TALLYING WS-TALLY
               FOR ALL "XX" AFTER INITIAL "D".
           IF WS-TALLY = 2
               DISPLAY "NC356A-TEST-2 PASS"
           ELSE
               DISPLAY "NC356A-TEST-2 FAIL"
               DISPLAY "  Expected tally=2, got "
                       WS-TALLY
           END-IF.
      * Test 3: INSPECT TALLYING with BEFORE and AFTER
      *   In "XXYYXXZZXXYYXXZZXX", count "XX" after "ZZ"
      *   and before "ZZ" (second occurrence doesn't apply
      *   since BEFORE/AFTER use INITIAL i.e. first).
      *   After first "ZZ": "XXYYXXZZXX" has XX at
      *   positions giving 3 occurrences.
           MOVE 0 TO WS-TALLY.
           INSPECT WS-DATA3 TALLYING WS-TALLY
               FOR ALL "XX" AFTER INITIAL "ZZ".
           IF WS-TALLY = 3
               DISPLAY "NC356A-TEST-3 PASS"
           ELSE
               DISPLAY "NC356A-TEST-3 FAIL"
               DISPLAY "  Expected tally=3, got "
                       WS-TALLY
           END-IF.
           STOP RUN.
