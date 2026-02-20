       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC220A.
      *
      * NIST CCVS-style test: CALL WITH NUMERIC PARAMETERS
      * Tests passing DISPLAY numeric parameters to a subprogram
      * that adds 100 and returns the result.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DISP-NUM           PIC 9(6)  VALUE 0.
       01  WS-DISP-RESULT        PIC 9(8)  VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: PASS 123456, ADD 100 = 123556
           MOVE 123456 TO WS-DISP-NUM.
           MOVE 0 TO WS-DISP-RESULT.
           CALL "IC220A-SUB" USING WS-DISP-NUM
                                    WS-DISP-RESULT.
           IF WS-DISP-RESULT = 123556
               DISPLAY "IC220A-TEST-1 PASS"
           ELSE
               DISPLAY "IC220A-TEST-1 FAIL"
               DISPLAY "  Expected 123556, got " WS-DISP-RESULT
           END-IF.
      * Test 2: PASS 500, ADD 100 = 600
           MOVE 500 TO WS-DISP-NUM.
           MOVE 0 TO WS-DISP-RESULT.
           CALL "IC220A-SUB" USING WS-DISP-NUM
                                    WS-DISP-RESULT.
           IF WS-DISP-RESULT = 600
               DISPLAY "IC220A-TEST-2 PASS"
           ELSE
               DISPLAY "IC220A-TEST-2 FAIL"
               DISPLAY "  Expected 600, got " WS-DISP-RESULT
           END-IF.
      * Test 3: PASS 50, ADD 100 = 150
           MOVE 50 TO WS-DISP-NUM.
           MOVE 0 TO WS-DISP-RESULT.
           CALL "IC220A-SUB" USING WS-DISP-NUM
                                    WS-DISP-RESULT.
           IF WS-DISP-RESULT = 150
               DISPLAY "IC220A-TEST-3 PASS"
           ELSE
               DISPLAY "IC220A-TEST-3 FAIL"
               DISPLAY "  Expected 150, got " WS-DISP-RESULT
           END-IF.
           STOP RUN.
