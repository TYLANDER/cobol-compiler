       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC217A.
      *
      * NIST CCVS-style test: CALL WITH RESULT VIA USING
      * Tests calling a subprogram that adds two numbers and
      * returns the result via a third USING parameter.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RETURN-VAL         PIC 9(8)  VALUE 0.
       01  WS-INPUT-A            PIC 9(4)  VALUE 0.
       01  WS-INPUT-B            PIC 9(4)  VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: 100 + 200 = 300
           MOVE 0 TO WS-RETURN-VAL.
           MOVE 100 TO WS-INPUT-A.
           MOVE 200 TO WS-INPUT-B.
           CALL "IC217A-SUB" USING WS-INPUT-A WS-INPUT-B
               WS-RETURN-VAL.
           IF WS-RETURN-VAL = 300
               DISPLAY "IC217A-TEST-1 PASS"
           ELSE
               DISPLAY "IC217A-TEST-1 FAIL"
               DISPLAY "  Expected 300, got " WS-RETURN-VAL
           END-IF.
      * Test 2: 1500 + 2500 = 4000
           MOVE 0 TO WS-RETURN-VAL.
           MOVE 1500 TO WS-INPUT-A.
           MOVE 2500 TO WS-INPUT-B.
           CALL "IC217A-SUB" USING WS-INPUT-A WS-INPUT-B
               WS-RETURN-VAL.
           IF WS-RETURN-VAL = 4000
               DISPLAY "IC217A-TEST-2 PASS"
           ELSE
               DISPLAY "IC217A-TEST-2 FAIL"
               DISPLAY "  Expected 4000, got " WS-RETURN-VAL
           END-IF.
      * Test 3: 0 + 0 = 0
           MOVE 99 TO WS-RETURN-VAL.
           MOVE 0 TO WS-INPUT-A.
           MOVE 0 TO WS-INPUT-B.
           CALL "IC217A-SUB" USING WS-INPUT-A WS-INPUT-B
               WS-RETURN-VAL.
           IF WS-RETURN-VAL = 0
               DISPLAY "IC217A-TEST-3 PASS"
           ELSE
               DISPLAY "IC217A-TEST-3 FAIL"
               DISPLAY "  Expected 0, got " WS-RETURN-VAL
           END-IF.
           STOP RUN.
