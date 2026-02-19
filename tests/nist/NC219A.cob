       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC219A.
      *
      * NIST CCVS-style test: REDEFINES clause
      * Tests REDEFINES for different interpretations of
      * the same storage area (numeric vs alpha, group vs
      * elementary).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM-FIELD    PIC 9(6)  VALUE 123456.
       01 WS-NUM-REDEF    REDEFINES WS-NUM-FIELD
                           PIC X(6).
       01 WS-GROUP-ORIG.
           05 WS-PART-A   PIC X(3)  VALUE "ABC".
           05 WS-PART-B   PIC X(3)  VALUE "DEF".
       01 WS-GROUP-REDEF  REDEFINES WS-GROUP-ORIG.
           05 WS-WHOLE     PIC X(6).
       01 WS-DATE-NUM     PIC 9(8) VALUE ZEROS.
       01 WS-DATE-REDEF   REDEFINES WS-DATE-NUM.
           05 WS-YEAR     PIC 9(4).
           05 WS-MONTH    PIC 9(2).
           05 WS-DAY      PIC 9(2).
       PROCEDURE DIVISION.
      * Test 1: REDEFINES numeric as alphanumeric
      *   WS-NUM-FIELD = 123456
      *   WS-NUM-REDEF (same storage) = "123456" as text
           IF WS-NUM-REDEF = "123456"
               DISPLAY "NC219A-TEST-1 PASS"
           ELSE
               DISPLAY "NC219A-TEST-1 FAIL"
               DISPLAY "  REDEF=>" WS-NUM-REDEF "<"
           END-IF.
      * Test 2: REDEFINES group as single field
      *   WS-PART-A = "ABC", WS-PART-B = "DEF"
      *   WS-WHOLE (redefines group) = "ABCDEF"
           IF WS-WHOLE = "ABCDEF"
               DISPLAY "NC219A-TEST-2 PASS"
           ELSE
               DISPLAY "NC219A-TEST-2 FAIL"
               DISPLAY "  WHOLE=>" WS-WHOLE "<"
           END-IF.
      * Test 3: REDEFINES to split numeric into parts
      *   Move 20250115 to WS-DATE-NUM
      *   Then WS-YEAR=2025, WS-MONTH=01, WS-DAY=15
           MOVE 20250115 TO WS-DATE-NUM.
           IF WS-YEAR = 2025
               AND WS-MONTH = 1
               AND WS-DAY = 15
               DISPLAY "NC219A-TEST-3 PASS"
           ELSE
               DISPLAY "NC219A-TEST-3 FAIL"
               DISPLAY "  YEAR=" WS-YEAR
                   " MONTH=" WS-MONTH
                   " DAY=" WS-DAY
           END-IF.
           STOP RUN.
