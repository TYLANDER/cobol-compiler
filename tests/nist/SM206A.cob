       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM206A.
      *
      * NIST CCVS-style test: REPLACE ... OFF statement
      * Tests that REPLACE is active until REPLACE OFF,
      * after which no further substitution occurs.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM206A-CPY.
       PROCEDURE DIVISION.
       REPLACE ==XVAR== BY ==SM206A-VAL==
               ==XNUM== BY ==SM206A-NUM==.
      * Test 1: REPLACE is active - XVAR becomes SM206A-VAL
           MOVE "ACTIVE    " TO XVAR.
           IF SM206A-VAL = "ACTIVE    "
               DISPLAY "SM206A-TEST-1 PASS"
           ELSE
               DISPLAY "SM206A-TEST-1 FAIL"
               DISPLAY "  VAL=>" SM206A-VAL "<"
           END-IF.
      * Test 2: REPLACE is active - XNUM becomes SM206A-NUM
           MOVE 3456 TO XNUM.
           IF SM206A-NUM = 3456
               DISPLAY "SM206A-TEST-2 PASS"
           ELSE
               DISPLAY "SM206A-TEST-2 FAIL"
               DISPLAY "  NUM=" SM206A-NUM
           END-IF.
       REPLACE OFF.
      * Test 3: After REPLACE OFF - use real names directly
           MOVE "OFF-STATE " TO SM206A-CHK.
           IF SM206A-CHK = "OFF-STATE "
               DISPLAY "SM206A-TEST-3 PASS"
           ELSE
               DISPLAY "SM206A-TEST-3 FAIL"
               DISPLAY "  CHK=>" SM206A-CHK "<"
           END-IF.
           STOP RUN.
