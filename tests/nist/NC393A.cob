       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC393A.
      *
      * NIST CCVS-style test: JUSTIFIED RIGHT clause
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-JUST-RIGHT      PIC X(10) JUSTIFIED RIGHT.
       01 WS-NORMAL          PIC X(10).
       01 WS-SOURCE           PIC X(5) VALUE "HELLO".
       PROCEDURE DIVISION.
      * Test 1: JUST RIGHT moves data right-aligned
           MOVE "ABC" TO WS-JUST-RIGHT.
           IF WS-JUST-RIGHT = "       ABC"
               DISPLAY "NC393A-TEST-1 PASS"
           ELSE
               DISPLAY "NC393A-TEST-1 FAIL"
               DISPLAY "  Got [" WS-JUST-RIGHT "]"
           END-IF.
      * Test 2: Normal (left-justified) for comparison
           MOVE "ABC" TO WS-NORMAL.
           IF WS-NORMAL = "ABC       "
               DISPLAY "NC393A-TEST-2 PASS"
           ELSE
               DISPLAY "NC393A-TEST-2 FAIL"
               DISPLAY "  Got [" WS-NORMAL "]"
           END-IF.
      * Test 3: JUST RIGHT with full-length data
           MOVE "1234567890" TO WS-JUST-RIGHT.
           IF WS-JUST-RIGHT = "1234567890"
               DISPLAY "NC393A-TEST-3 PASS"
           ELSE
               DISPLAY "NC393A-TEST-3 FAIL"
               DISPLAY "  Got [" WS-JUST-RIGHT "]"
           END-IF.
           STOP RUN.
