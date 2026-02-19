       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC163A.
      *
      * NIST CCVS-style test: SEARCH with AT END
      * Tests SEARCH (linear) on an OCCURS table
      * matching and AT END fallback.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
           05 WS-ENTRY OCCURS 5 TIMES
               INDEXED BY WS-IDX.
               10 WS-VALUE PIC 9(3).
       01 WS-FOUND         PIC 9(3)  VALUE ZEROS.
       01 WS-FLAG           PIC 9     VALUE 0.
       PROCEDURE DIVISION.
      * Populate table
           MOVE 10 TO WS-VALUE(1).
           MOVE 20 TO WS-VALUE(2).
           MOVE 30 TO WS-VALUE(3).
           MOVE 40 TO WS-VALUE(4).
           MOVE 50 TO WS-VALUE(5).
      * Test 1: SEARCH finds entry key=30
           MOVE ZEROS TO WS-FOUND.
           MOVE 0 TO WS-FLAG.
           SET WS-IDX TO 1.
           SEARCH WS-ENTRY
               AT END
                   MOVE 1 TO WS-FLAG
               WHEN WS-VALUE(WS-IDX) = 30
                   MOVE WS-VALUE(WS-IDX) TO WS-FOUND
           END-SEARCH.
           IF WS-FLAG = 0 AND WS-FOUND = 30
               DISPLAY "NC163A-TEST-1 PASS"
           ELSE
               DISPLAY "NC163A-TEST-1 FAIL"
               DISPLAY "  FLAG=" WS-FLAG
                   " FOUND=>" WS-FOUND "<"
           END-IF.
      * Test 2: SEARCH for non-existing key hits AT END
           MOVE ZEROS TO WS-FOUND.
           MOVE 0 TO WS-FLAG.
           SET WS-IDX TO 1.
           SEARCH WS-ENTRY
               AT END
                   MOVE 1 TO WS-FLAG
               WHEN WS-VALUE(WS-IDX) = 99
                   MOVE WS-VALUE(WS-IDX) TO WS-FOUND
           END-SEARCH.
           IF WS-FLAG = 1 AND WS-FOUND = 0
               DISPLAY "NC163A-TEST-2 PASS"
           ELSE
               DISPLAY "NC163A-TEST-2 FAIL"
               DISPLAY "  FLAG=" WS-FLAG
                   " FOUND=>" WS-FOUND "<"
           END-IF.
           STOP RUN.
