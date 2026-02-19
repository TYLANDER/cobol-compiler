       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM109A.
      *
      * NIST CCVS-style test: COPY REPLACING TRAILING
      * Tests COPY with REPLACING TRAILING to rename suffixes.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SM-TRAIL
           REPLACING TRAILING ==:SFX:== BY ==NEW==.
       PROCEDURE DIVISION.
      * Test 1: Fields named ITEM-NEW and COUNT-NEW exist
           IF ITEM-NEW = SPACES AND COUNT-NEW = 0
               DISPLAY "SM109A-TEST-1 PASS"
           ELSE
               DISPLAY "SM109A-TEST-1 FAIL"
               DISPLAY "  ITEM=>" ITEM-NEW "<"
               DISPLAY "  COUNT=" COUNT-NEW
           END-IF.
      * Test 2: Modify the fields and verify
           MOVE "HELLO" TO ITEM-NEW.
           MOVE 9876 TO COUNT-NEW.
           IF ITEM-NEW = "HELLO" AND COUNT-NEW = 9876
               DISPLAY "SM109A-TEST-2 PASS"
           ELSE
               DISPLAY "SM109A-TEST-2 FAIL"
               DISPLAY "  ITEM=>" ITEM-NEW "<"
               DISPLAY "  COUNT=" COUNT-NEW
           END-IF.
           STOP RUN.
