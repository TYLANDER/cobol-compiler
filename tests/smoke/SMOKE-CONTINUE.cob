       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-CONTINUE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-X PIC 9(2) VALUE 5.
       01 WS-Y PIC 9(2) VALUE 10.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
      *-----------------------------------------------------------------
      * Test 1: CONTINUE in ELSE branch of IF
      *-----------------------------------------------------------------
           IF WS-X < WS-Y
               DISPLAY "IF-PASS"
           ELSE
               CONTINUE
           END-IF.

      *-----------------------------------------------------------------
      * Test 2: CONTINUE in THEN branch (do nothing), ELSE does work
      *-----------------------------------------------------------------
           IF WS-X > WS-Y
               CONTINUE
           ELSE
               DISPLAY "ELSE-PASS"
           END-IF.

      *-----------------------------------------------------------------
      * Test 3: EVALUATE with WHEN OTHER CONTINUE
      *-----------------------------------------------------------------
           EVALUATE WS-X
               WHEN 5
                   DISPLAY "EVAL-PASS"
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *-----------------------------------------------------------------
      * Test 4: EVALUATE where matching WHEN uses CONTINUE
      *-----------------------------------------------------------------
           EVALUATE WS-Y
               WHEN 1
                   DISPLAY "EVAL-FAIL"
               WHEN 10
                   CONTINUE
               WHEN OTHER
                   DISPLAY "EVAL-FAIL"
           END-EVALUATE.

      *-----------------------------------------------------------------
      * Test 5: Empty paragraph - PERFORM it, should be a no-op
      *-----------------------------------------------------------------
           PERFORM EMPTY-PARA.
           DISPLAY "AFTER-EMPTY".

      *-----------------------------------------------------------------
      * Test 6: CONTINUE as standalone sentence
      *-----------------------------------------------------------------
           CONTINUE.
           DISPLAY "FINAL-PASS".
           STOP RUN.

      *-----------------------------------------------------------------
      * Empty paragraph with no statements
      *-----------------------------------------------------------------
       EMPTY-PARA.
