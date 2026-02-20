       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC378A.
      *
      * NIST CCVS-style test: REDEFINES with different PIC
      * types.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ALPHA-FIELD  PIC X(5) VALUE "12345".
       01 WS-ALPHA-AS-NUM REDEFINES WS-ALPHA-FIELD
                           PIC 9(5).
       01 WS-NUM-FIELD    PIC 9(4) VALUE 6789.
       01 WS-NUM-AS-ALPHA REDEFINES WS-NUM-FIELD
                           PIC X(4).
       01 WS-GROUP-ORIG.
           05 WS-ORIG-A   PIC X(3) VALUE "ABC".
           05 WS-ORIG-B   PIC X(3) VALUE "123".
       01 WS-GROUP-REDEF  REDEFINES WS-GROUP-ORIG.
           05 WS-REDEF-X  PIC X(2).
           05 WS-REDEF-Y  PIC X(4).
       01 WS-RESULT       PIC 9(5) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Alphanumeric "12345" redefined as numeric
           MOVE WS-ALPHA-AS-NUM TO WS-RESULT.
           IF WS-RESULT = 12345
               DISPLAY "NC378A-TEST-1 PASS"
           ELSE
               DISPLAY "NC378A-TEST-1 FAIL"
               DISPLAY "  Expected 12345, got " WS-RESULT
           END-IF.
      * Test 2: Numeric 6789 redefined as alphanumeric
           IF WS-NUM-AS-ALPHA = "6789"
               DISPLAY "NC378A-TEST-2 PASS"
           ELSE
               DISPLAY "NC378A-TEST-2 FAIL"
               DISPLAY "  Expected 6789, got "
                   WS-NUM-AS-ALPHA
           END-IF.
      * Test 3: Group redefines with different layout
      * "ABC123" redefined as X(2) + X(4)
           IF WS-REDEF-X = "AB" AND WS-REDEF-Y = "C123"
               DISPLAY "NC378A-TEST-3 PASS"
           ELSE
               DISPLAY "NC378A-TEST-3 FAIL"
               DISPLAY "  REDEF-X=" WS-REDEF-X
               DISPLAY "  REDEF-Y=" WS-REDEF-Y
           END-IF.
           STOP RUN.
