       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC198A.
      *
      * NIST CCVS-style test: MOVE CORRESPONDING with mixed types
      * Tests MOVE CORRESPONDING between group items that have
      * a mix of alphanumeric and numeric fields, verifying that
      * only matching-name fields are moved and non-matching fields
      * remain unchanged.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC-GRP.
           05 WS-ITEM-NAME  PIC X(10) VALUE "WIDGET".
           05 WS-ITEM-QTY   PIC 9(4)  VALUE 500.
           05 WS-ITEM-PRICE PIC 9(4)V99 VALUE 12.50.
           05 WS-ITEM-CODE  PIC X(3)  VALUE "A01".
       01 WS-DST-GRP.
           05 WS-ITEM-NAME  PIC X(10) VALUE SPACES.
           05 WS-ITEM-QTY   PIC 9(4)  VALUE ZEROS.
           05 WS-ITEM-PRICE PIC 9(4)V99 VALUE ZEROS.
           05 WS-ITEM-LOC   PIC X(5)  VALUE "WARE1".
       01 WS-GRP-ALPHA-SRC.
           05 WS-FIRST      PIC X(8) VALUE "ALICE".
           05 WS-LAST       PIC X(8) VALUE "JOHNSON".
           05 WS-MIDDLE     PIC X(8) VALUE "MAE".
       01 WS-GRP-ALPHA-DST.
           05 WS-FIRST      PIC X(8) VALUE SPACES.
           05 WS-LAST       PIC X(8) VALUE SPACES.
           05 WS-TITLE      PIC X(4) VALUE "MRS.".
       PROCEDURE DIVISION.
      * Test 1: MOVE CORRESPONDING with mixed numeric/alpha fields
      *   Matching: WS-ITEM-NAME, WS-ITEM-QTY, WS-ITEM-PRICE
      *   Non-matching in SRC: WS-ITEM-CODE
      *   Non-matching in DST: WS-ITEM-LOC (should stay "WARE1")
           MOVE CORRESPONDING WS-SRC-GRP TO WS-DST-GRP.
           IF WS-ITEM-NAME OF WS-DST-GRP = "WIDGET"
               AND WS-ITEM-QTY OF WS-DST-GRP = 500
               AND WS-ITEM-PRICE OF WS-DST-GRP = 12.50
               AND WS-ITEM-LOC = "WARE1"
               DISPLAY "NC198A-TEST-1 PASS"
           ELSE
               DISPLAY "NC198A-TEST-1 FAIL"
               DISPLAY "  NAME=>"
                   WS-ITEM-NAME OF WS-DST-GRP "<"
               DISPLAY "  QTY="
                   WS-ITEM-QTY OF WS-DST-GRP
               DISPLAY "  PRICE="
                   WS-ITEM-PRICE OF WS-DST-GRP
               DISPLAY "  LOC=>" WS-ITEM-LOC "<"
           END-IF.
      * Test 2: MOVE CORRESPONDING with alphanumeric-only fields
      *   Matching: WS-FIRST, WS-LAST
      *   Non-matching in SRC: WS-MIDDLE
      *   Non-matching in DST: WS-TITLE (should stay "MRS.")
           MOVE CORRESPONDING WS-GRP-ALPHA-SRC
               TO WS-GRP-ALPHA-DST.
           IF WS-FIRST OF WS-GRP-ALPHA-DST = "ALICE"
               AND WS-LAST OF WS-GRP-ALPHA-DST = "JOHNSON"
               AND WS-TITLE = "MRS."
               DISPLAY "NC198A-TEST-2 PASS"
           ELSE
               DISPLAY "NC198A-TEST-2 FAIL"
               DISPLAY "  FIRST=>"
                   WS-FIRST OF WS-GRP-ALPHA-DST "<"
               DISPLAY "  LAST=>"
                   WS-LAST OF WS-GRP-ALPHA-DST "<"
               DISPLAY "  TITLE=>" WS-TITLE "<"
           END-IF.
      * Test 3: Verify source group is unchanged after MOVE CORR
           IF WS-ITEM-NAME OF WS-SRC-GRP = "WIDGET"
               AND WS-ITEM-QTY OF WS-SRC-GRP = 500
               AND WS-ITEM-CODE = "A01"
               DISPLAY "NC198A-TEST-3 PASS"
           ELSE
               DISPLAY "NC198A-TEST-3 FAIL"
               DISPLAY "  SRC NAME=>"
                   WS-ITEM-NAME OF WS-SRC-GRP "<"
               DISPLAY "  SRC QTY="
                   WS-ITEM-QTY OF WS-SRC-GRP
               DISPLAY "  SRC CODE=>" WS-ITEM-CODE "<"
           END-IF.
           STOP RUN.
