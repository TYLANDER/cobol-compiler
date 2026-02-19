       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC127A.
      *
      * NIST CCVS-style test: INITIALIZE statement
      * Tests INITIALIZE group item, INITIALIZE with REPLACING
      * NUMERIC BY literal, and INITIALIZE elementary items.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP.
           05 WS-GRP-ALPHA PIC X(5)  VALUE "HELLO".
           05 WS-GRP-NUM   PIC 9(4)  VALUE 9999.
           05 WS-GRP-ALP2  PIC X(3)  VALUE "XYZ".
       01 WS-GROUP2.
           05 WS-G2-ALPHA  PIC X(5)  VALUE "AAAAA".
           05 WS-G2-NUM    PIC 9(4)  VALUE 1111.
       01 WS-ELEM-ALPHA    PIC X(6)  VALUE "FILLED".
       01 WS-ELEM-NUM      PIC 9(4)  VALUE 5555.
       PROCEDURE DIVISION.
      * Test 1: INITIALIZE a group item
      *   Numeric fields get zeros, alphanumeric get spaces
           INITIALIZE WS-GROUP.
           IF WS-GRP-ALPHA = SPACES
             AND WS-GRP-NUM = 0
             AND WS-GRP-ALP2 = SPACES
               DISPLAY "NC127A-TEST-1 PASS"
           ELSE
               DISPLAY "NC127A-TEST-1 FAIL"
               DISPLAY "  ALPHA=>" WS-GRP-ALPHA "<"
               DISPLAY "  NUM=" WS-GRP-NUM
               DISPLAY "  ALP2=>" WS-GRP-ALP2 "<"
           END-IF.
      * Test 2: INITIALIZE with REPLACING NUMERIC BY literal
      *   Numeric fields get 42, alpha fields get spaces
           MOVE "BBBBB" TO WS-G2-ALPHA.
           MOVE 2222 TO WS-G2-NUM.
           INITIALIZE WS-GROUP2
               REPLACING NUMERIC DATA BY 42.
           IF WS-G2-ALPHA = SPACES AND WS-G2-NUM = 42
               DISPLAY "NC127A-TEST-2 PASS"
           ELSE
               DISPLAY "NC127A-TEST-2 FAIL"
               DISPLAY "  ALPHA=>" WS-G2-ALPHA "<"
               DISPLAY "  NUM=" WS-G2-NUM
           END-IF.
      * Test 3: INITIALIZE elementary items separately
      *   Alpha elementary gets spaces, numeric elementary gets 0
           MOVE "FILLED" TO WS-ELEM-ALPHA.
           MOVE 5555 TO WS-ELEM-NUM.
           INITIALIZE WS-ELEM-ALPHA.
           INITIALIZE WS-ELEM-NUM.
           IF WS-ELEM-ALPHA = SPACES AND WS-ELEM-NUM = 0
               DISPLAY "NC127A-TEST-3 PASS"
           ELSE
               DISPLAY "NC127A-TEST-3 FAIL"
               DISPLAY "  ALPHA=>" WS-ELEM-ALPHA "<"
               DISPLAY "  NUM=" WS-ELEM-NUM
           END-IF.
           STOP RUN.
