       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-DEEDIT.
      *
      * Test MOVE with numeric-edited source fields (de-editing).
      * When moving a numeric-edited field to a numeric field,
      * the compiler must strip editing characters and extract
      * the raw numeric value.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Source numeric-edited fields
       01 WS-EDITED-1   PIC Z(4)9.99.
       01 WS-EDITED-2   PIC ZZ,ZZ9.
       01 WS-EDITED-3   PIC ZZ9.
      * Target numeric fields
       01 WS-NUM-1       PIC 9(5)V99.
       01 WS-NUM-2       PIC 9(5).
       01 WS-NUM-3       PIC 9(3).
      * Source plain numeric for setup
       01 WS-AMOUNT      PIC 9(5)V99 VALUE 1234.56.
       01 WS-INTVAL      PIC 9(5) VALUE 42.
       01 WS-SMALL       PIC 9(3) VALUE 7.
       PROCEDURE DIVISION.
      *
      * TEST 1: Numeric-edited with zero suppress and decimal
      *         MOVE numeric to edited, then de-edit back.
      *
           MOVE WS-AMOUNT TO WS-EDITED-1.
           MOVE WS-EDITED-1 TO WS-NUM-1.
           IF WS-NUM-1 = 1234.56
               DISPLAY "PASS DEEDIT-1"
           ELSE
               DISPLAY "FAIL DEEDIT-1: [" WS-NUM-1 "]"
           END-IF.
      *
      * TEST 2: Zero-suppressed integer field with commas
      *
           MOVE WS-INTVAL TO WS-EDITED-2.
           MOVE WS-EDITED-2 TO WS-NUM-2.
           IF WS-NUM-2 = 42
               DISPLAY "PASS DEEDIT-2"
           ELSE
               DISPLAY "FAIL DEEDIT-2: [" WS-NUM-2 "]"
           END-IF.
      *
      * TEST 3: Small zero-suppressed field
      *
           MOVE WS-SMALL TO WS-EDITED-3.
           MOVE WS-EDITED-3 TO WS-NUM-3.
           IF WS-NUM-3 = 7
               DISPLAY "PASS DEEDIT-3"
           ELSE
               DISPLAY "FAIL DEEDIT-3: [" WS-NUM-3 "]"
           END-IF.
           STOP RUN.
