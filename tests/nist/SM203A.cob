       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM203A.
      *
      * NIST CCVS-style test: REPLACE statement changing procedure names
      * Tests standalone REPLACE for procedure-name substitution.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM203A-CPY.
       PROCEDURE DIVISION.
       REPLACE ==PROC-INIT== BY ==INIT-PARAGRAPH==
               ==PROC-CALC== BY ==CALC-PARAGRAPH==.
       MAIN-PROGRAM.
           PERFORM PROC-INIT.
           PERFORM PROC-CALC.
      * Test 1: INIT paragraph executed via replaced name
           IF SM203A-RESULT = "INIT-DONE "
               DISPLAY "SM203A-TEST-1 PASS"
           ELSE
               DISPLAY "SM203A-TEST-1 FAIL"
               DISPLAY "  RESULT=>" SM203A-RESULT "<"
           END-IF.
      * Test 2: CALC paragraph executed via replaced name
           IF SM203A-COUNT = 42
               DISPLAY "SM203A-TEST-2 PASS"
           ELSE
               DISPLAY "SM203A-TEST-2 FAIL"
               DISPLAY "  COUNT=" SM203A-COUNT
           END-IF.
       REPLACE OFF.
      * Test 3: After REPLACE OFF, verify data still intact
           IF SM203A-RESULT = "INIT-DONE " AND SM203A-COUNT = 42
               DISPLAY "SM203A-TEST-3 PASS"
           ELSE
               DISPLAY "SM203A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
       INIT-PARAGRAPH.
           MOVE "INIT-DONE " TO SM203A-RESULT.
       CALC-PARAGRAPH.
           MOVE 42 TO SM203A-COUNT.
