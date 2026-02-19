       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC349A.
      *
      * NIST CCVS-style test: REDEFINES on group items
      * Tests REDEFINES to overlay different data descriptions
      * on the same storage area. Level-66 RENAMES is not yet
      * supported, so this tests the supported REDEFINES feature
      * which provides similar data-aliasing capability.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATE-NUM     PIC 9(8) VALUE 20260219.
       01 WS-DATE-PARTS REDEFINES WS-DATE-NUM.
           05 WS-YEAR      PIC 9(4).
           05 WS-MONTH     PIC 9(2).
           05 WS-DAY       PIC 9(2).
       01 WS-FULL-NAME    PIC X(15) VALUE "JOHN SMITH     ".
       01 WS-NAME-PARTS REDEFINES WS-FULL-NAME.
           05 WS-FIRST     PIC X(5).
           05 WS-LAST      PIC X(10).
       01 WS-AMOUNT       PIC 9(4) VALUE 1234.
       01 WS-AMT-X REDEFINES WS-AMOUNT PIC X(4).
       PROCEDURE DIVISION.
      * Test 1: REDEFINES numeric as group with parts
      *   20260219 => YEAR=2026, MONTH=02, DAY=19
           IF WS-YEAR = 2026 AND WS-MONTH = 02 AND WS-DAY = 19
               DISPLAY "NC349A-TEST-1 PASS"
           ELSE
               DISPLAY "NC349A-TEST-1 FAIL"
               DISPLAY "  Y=" WS-YEAR " M=" WS-MONTH
                   " D=" WS-DAY
           END-IF.
      * Test 2: REDEFINES alphanumeric as group
      *   "JOHN SMITH     " => FIRST="JOHN ", LAST="SMITH     "
           IF WS-FIRST = "JOHN "
               AND WS-LAST = "SMITH     "
               DISPLAY "NC349A-TEST-2 PASS"
           ELSE
               DISPLAY "NC349A-TEST-2 FAIL"
               DISPLAY "  FIRST=>" WS-FIRST "<"
               DISPLAY "  LAST=>" WS-LAST "<"
           END-IF.
      * Test 3: REDEFINES numeric as alphanumeric
      *   Move to numeric, read back via alphanumeric alias
           MOVE 5678 TO WS-AMOUNT.
           IF WS-AMT-X = "5678"
               DISPLAY "NC349A-TEST-3 PASS"
           ELSE
               DISPLAY "NC349A-TEST-3 FAIL"
               DISPLAY "  Expected '5678', got >" WS-AMT-X "<"
           END-IF.
           STOP RUN.
