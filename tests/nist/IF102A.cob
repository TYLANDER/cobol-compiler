       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF102A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MIXED            PIC X(10) VALUE "HeLLo WoRd".
       01  WS-RESULT           PIC X(10).
       01  WS-RESULT5          PIC X(5).
       PROCEDURE DIVISION.
      *    TEST 1: UPPER-CASE of literal "hello" = "HELLO"
           MOVE FUNCTION UPPER-CASE("hello")
               TO WS-RESULT5
           IF WS-RESULT5 = "HELLO"
               DISPLAY "IF102A-TEST-1 PASS"
           ELSE
               DISPLAY "IF102A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: UPPER-CASE of mixed-case field
           MOVE FUNCTION UPPER-CASE(WS-MIXED)
               TO WS-RESULT
           IF WS-RESULT = "HELLO WORD"
               DISPLAY "IF102A-TEST-2 PASS"
           ELSE
               DISPLAY "IF102A-TEST-2 FAIL"
           END-IF.
      *    TEST 3: UPPER-CASE preserves non-alpha characters
           MOVE FUNCTION UPPER-CASE("ab-12.cd")
               TO WS-RESULT
           IF WS-RESULT = "AB-12.CD  "
               DISPLAY "IF102A-TEST-3 PASS"
           ELSE
               DISPLAY "IF102A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
