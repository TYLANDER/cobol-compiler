       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC114A.
      *
      * NIST CCVS-style test: Data item features
      * Tests level 88 condition names, REDEFINES clause,
      * and OCCURS with subscripts.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STATUS   PIC 9 VALUE 0.
           88 WS-ACTIVE   VALUE 1.
           88 WS-INACTIVE VALUE 0.
       01 WS-NUM-FIELD PIC 9(4) VALUE 1234.
       01 WS-ALPHA-FIELD REDEFINES WS-NUM-FIELD PIC X(4).
       01 WS-TABLE.
           05 WS-ITEM PIC 9(2) OCCURS 5 TIMES.
       01 WS-SUM      PIC 9(4) VALUE ZEROS.
       01 WS-I        PIC 9(2) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Level 88 condition names
           MOVE 0 TO WS-STATUS.
           IF WS-INACTIVE
               MOVE 1 TO WS-STATUS
           END-IF.
           IF WS-ACTIVE
               DISPLAY "NC114A-TEST-1 PASS"
           ELSE
               DISPLAY "NC114A-TEST-1 FAIL"
               DISPLAY "  WS-STATUS=" WS-STATUS
           END-IF.
      * Test 2: REDEFINES clause
           MOVE 1234 TO WS-NUM-FIELD.
           IF WS-ALPHA-FIELD = "1234"
               DISPLAY "NC114A-TEST-2 PASS"
           ELSE
               DISPLAY "NC114A-TEST-2 FAIL"
               DISPLAY "  Expected 1234, got >"
                   WS-ALPHA-FIELD "<"
           END-IF.
      * Test 3: OCCURS with subscripts
           MOVE 10 TO WS-ITEM(1).
           MOVE 20 TO WS-ITEM(2).
           MOVE 30 TO WS-ITEM(3).
           MOVE 40 TO WS-ITEM(4).
           MOVE 50 TO WS-ITEM(5).
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 5
               ADD WS-ITEM(WS-I) TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 150
               DISPLAY "NC114A-TEST-3 PASS"
           ELSE
               DISPLAY "NC114A-TEST-3 FAIL"
               DISPLAY "  Expected 150, got " WS-SUM
           END-IF.
           STOP RUN.
