       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC392A.
      *
      * NIST CCVS-style test: Alphanumeric comparison rules
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SHORT           PIC X(3)  VALUE "ABC".
       01 WS-LONG            PIC X(6)  VALUE "ABC   ".
       01 WS-LONG2           PIC X(6)  VALUE "ABCDEF".
       01 WS-A               PIC X(5)  VALUE "AAAAA".
       01 WS-B               PIC X(5)  VALUE "AAAAB".
       PROCEDURE DIVISION.
      * Test 1: Short vs long with trailing spaces (equal)
           IF WS-SHORT = WS-LONG
               DISPLAY "NC392A-TEST-1 PASS"
           ELSE
               DISPLAY "NC392A-TEST-1 FAIL"
               DISPLAY "  ABC should equal ABC-spaces"
           END-IF.
      * Test 2: Short vs long with trailing chars (not equal)
           IF WS-SHORT < WS-LONG2
               DISPLAY "NC392A-TEST-2 PASS"
           ELSE
               DISPLAY "NC392A-TEST-2 FAIL"
               DISPLAY "  ABC should be < ABCDEF"
           END-IF.
      * Test 3: Character-by-character comparison
           IF WS-A < WS-B
               DISPLAY "NC392A-TEST-3 PASS"
           ELSE
               DISPLAY "NC392A-TEST-3 FAIL"
               DISPLAY "  AAAAA should be < AAAAB"
           END-IF.
           STOP RUN.
