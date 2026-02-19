       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC208A.
      *
      * NIST CCVS-style test: CALL with BY REFERENCE modification
      * Tests that a subprogram can modify multiple BY REFERENCE
      * parameters and the caller sees all changes.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME   PIC X(10) VALUE "BEFORE    ".
       01 WS-CODE   PIC 9(4) VALUE 0.
       01 WS-STATUS PIC X(8) VALUE "INITIAL ".
       PROCEDURE DIVISION.
      * Test 1: Subprogram modifies alpha BY REFERENCE
           CALL "IC208A-SUB" USING BY REFERENCE WS-NAME
                                   BY REFERENCE WS-CODE
                                   BY REFERENCE WS-STATUS.
           IF WS-NAME = "AFTER     "
               DISPLAY "IC208A-TEST-1 PASS"
           ELSE
               DISPLAY "IC208A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-NAME "<"
           END-IF.
      * Test 2: Subprogram modifies numeric BY REFERENCE
           IF WS-CODE = 4444
               DISPLAY "IC208A-TEST-2 PASS"
           ELSE
               DISPLAY "IC208A-TEST-2 FAIL"
               DISPLAY "  CODE=" WS-CODE
           END-IF.
      * Test 3: Subprogram modifies third alpha BY REFERENCE
           IF WS-STATUS = "COMPLETE"
               DISPLAY "IC208A-TEST-3 PASS"
           ELSE
               DISPLAY "IC208A-TEST-3 FAIL"
               DISPLAY "  STATUS=>" WS-STATUS "<"
           END-IF.
           STOP RUN.
