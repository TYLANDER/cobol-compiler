       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC105A.
      *
      * NIST CCVS-style test: CALL with string processing
      * Tests passing an alphanumeric field BY REFERENCE to a
      * subprogram that modifies it.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MSG PIC X(10) VALUE "HELLO MAIN".
       PROCEDURE DIVISION.
      * Test 1: String parameter passing BY REFERENCE works
           CALL "IC105A-SUB" USING WS-MSG.
           IF WS-MSG = "HELLO BACK"
               DISPLAY "IC105A-TEST-1 PASS"
           ELSE
               DISPLAY "IC105A-TEST-1 FAIL"
               DISPLAY "  MSG=>" WS-MSG "<"
           END-IF.
      * Test 2: Main program reads back modified string
           IF WS-MSG(1:5) = "HELLO"
               DISPLAY "IC105A-TEST-2 PASS"
           ELSE
               DISPLAY "IC105A-TEST-2 FAIL"
               DISPLAY "  SUBSTR=>" WS-MSG(1:5) "<"
           END-IF.
           STOP RUN.
