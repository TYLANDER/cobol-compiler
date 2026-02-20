       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC364A.
      *
      * NIST CCVS-style test: REDEFINES with different PIC types
      * Tests REDEFINES to overlay the same storage with numeric
      * and alphanumeric interpretations.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM-FIELD   PIC 9(6) VALUE 123456.
       01 WS-ALPHA-FIELD REDEFINES WS-NUM-FIELD PIC X(6).
       01 WS-GROUP-REC.
           05 WS-PART-A   PIC X(4) VALUE "ABCD".
           05 WS-PART-B   PIC X(4) VALUE "1234".
       01 WS-GROUP-ALT REDEFINES WS-GROUP-REC.
           05 WS-WHOLE    PIC X(8).
       01 WS-COMP-AREA   PIC 9(4) VALUE 9876.
       01 WS-COMP-ALT REDEFINES WS-COMP-AREA PIC X(4).
       01 WS-CHECK       PIC X(8) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Numeric field read as alphanumeric via REDEFINES
           IF WS-ALPHA-FIELD = "123456"
               DISPLAY "NC364A-TEST-1 PASS"
           ELSE
               DISPLAY "NC364A-TEST-1 FAIL"
               DISPLAY "  Expected 123456"
               DISPLAY "  Got      " WS-ALPHA-FIELD
           END-IF.
      * Test 2: Group REDEFINES reads two fields as one
           MOVE WS-WHOLE TO WS-CHECK.
           IF WS-CHECK = "ABCD1234"
               DISPLAY "NC364A-TEST-2 PASS"
           ELSE
               DISPLAY "NC364A-TEST-2 FAIL"
               DISPLAY "  Expected ABCD1234"
               DISPLAY "  Got      " WS-CHECK
           END-IF.
      * Test 3: MOVE to redefined field changes original
           MOVE "WXYZ" TO WS-COMP-ALT.
           IF WS-COMP-ALT = "WXYZ"
               DISPLAY "NC364A-TEST-3 PASS"
           ELSE
               DISPLAY "NC364A-TEST-3 FAIL"
               DISPLAY "  Expected WXYZ"
               DISPLAY "  Got " WS-COMP-ALT
           END-IF.
           STOP RUN.
