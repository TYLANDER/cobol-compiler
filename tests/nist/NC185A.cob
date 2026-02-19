       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC185A.
      *
      * NIST CCVS-style test: REDEFINES for alternate access
      * Tests REDEFINES clause to provide different PIC
      * interpretations of the same storage area, simulating
      * rename-like access patterns.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RECORD.
           05 WS-FULL-NAME   PIC X(20) VALUE SPACES.
           05 WS-CODE         PIC 9(4)  VALUE ZEROS.
           05 WS-AMOUNT       PIC 9(6)  VALUE ZEROS.
       01 WS-RECORD-ALT REDEFINES WS-RECORD.
           05 WS-FIRST-10     PIC X(10).
           05 WS-LAST-10      PIC X(10).
           05 WS-CODE-X       PIC X(4).
           05 WS-AMT-X        PIC X(6).
       01 WS-NUM-FIELD        PIC 9(6)  VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Write via original, read via REDEFINES
      *   Store name and read first 10 chars through alternate
           MOVE "JOHN DOE  JANE SMITH" TO WS-FULL-NAME.
           MOVE 1234 TO WS-CODE.
           IF WS-FIRST-10 = "JOHN DOE  "
               DISPLAY "NC185A-TEST-1 PASS"
           ELSE
               DISPLAY "NC185A-TEST-1 FAIL"
               DISPLAY "  Expected 'JOHN DOE  '"
               DISPLAY "  Got >" WS-FIRST-10 "<"
           END-IF.
      * Test 2: Read numeric field as alphanumeric via REDEFINES
      *   WS-CODE = 1234, WS-CODE-X should be "1234"
           IF WS-CODE-X = "1234"
               DISPLAY "NC185A-TEST-2 PASS"
           ELSE
               DISPLAY "NC185A-TEST-2 FAIL"
               DISPLAY "  Expected '1234', got >"
                   WS-CODE-X "<"
           END-IF.
      * Test 3: Write via REDEFINES, read via original
      *   Set WS-CODE-X as alphanumeric, read WS-CODE as numeric
           MOVE "5678" TO WS-CODE-X.
           IF WS-CODE = 5678
               DISPLAY "NC185A-TEST-3 PASS"
           ELSE
               DISPLAY "NC185A-TEST-3 FAIL"
               DISPLAY "  Expected 5678, got " WS-CODE
           END-IF.
           STOP RUN.
