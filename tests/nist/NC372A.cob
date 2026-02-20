       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC372A.
      *
      * NIST CCVS-style test: MULTIPLY/DIVIDE with GIVING.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(3)  VALUE 25.
       01 WS-B            PIC 9(3)  VALUE 4.
       01 WS-C            PIC 9(3)  VALUE 0.
       01 WS-D            PIC 9(3)  VALUE 0.
       01 WS-E            PIC 9(3)  VALUE 100.
       01 WS-F            PIC 9(3)  VALUE 8.
       01 WS-G            PIC 9(3)  VALUE 0.
       01 WS-REM          PIC 9(3)  VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: MULTIPLY 25 BY 4 GIVING C = 100
           MOVE 0 TO WS-C.
           MULTIPLY WS-A BY WS-B GIVING WS-C.
           IF WS-C = 100
               DISPLAY "NC372A-TEST-1 PASS"
           ELSE
               DISPLAY "NC372A-TEST-1 FAIL"
               DISPLAY "  Expected 100, got " WS-C
           END-IF.
      * Test 2: MULTIPLY 12 BY 9 GIVING D = 108
           MOVE 12 TO WS-A.
           MOVE 9 TO WS-B.
           MOVE 0 TO WS-D.
           MULTIPLY WS-A BY WS-B GIVING WS-D.
           IF WS-D = 108
               DISPLAY "NC372A-TEST-2 PASS"
           ELSE
               DISPLAY "NC372A-TEST-2 FAIL"
               DISPLAY "  Expected 108, got " WS-D
           END-IF.
      * Test 3: DIVIDE 100 BY 8 GIVING G REMAINDER REM
           MOVE 0 TO WS-G.
           MOVE 0 TO WS-REM.
           DIVIDE WS-E BY WS-F GIVING WS-G
               REMAINDER WS-REM.
           IF WS-G = 12 AND WS-REM = 4
               DISPLAY "NC372A-TEST-3 PASS"
           ELSE
               DISPLAY "NC372A-TEST-3 FAIL"
               DISPLAY "  Expected 12 rem 4"
               DISPLAY "  Got " WS-G " rem " WS-REM
           END-IF.
           STOP RUN.
