       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC287A.
      *
      * NIST CCVS-style test: INITIALIZE with REPLACING
      * Tests INITIALIZE REPLACING NUMERIC DATA BY and
      * REPLACING ALPHANUMERIC DATA BY on group items.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP1.
           05 WS-G1-ALPHA     PIC X(5) VALUE "HELLO".
           05 WS-G1-NUM       PIC 9(4) VALUE 9999.
           05 WS-G1-ALPHA2    PIC X(3) VALUE "XYZ".
       01 WS-GROUP2.
           05 WS-G2-ALPHA     PIC X(4) VALUE "TEST".
           05 WS-G2-NUM1      PIC 9(3) VALUE 111.
           05 WS-G2-NUM2      PIC 9(3) VALUE 222.
       01 WS-GROUP3.
           05 WS-G3-ALPHA     PIC X(6) VALUE "ABCDEF".
           05 WS-G3-NUM       PIC 9(4) VALUE 5555.
       PROCEDURE DIVISION.
      * Test 1: INITIALIZE REPLACING NUMERIC DATA BY literal
      *   Numeric fields get 42, alpha fields get default spaces
           MOVE "HELLO" TO WS-G1-ALPHA.
           MOVE 9999 TO WS-G1-NUM.
           MOVE "XYZ" TO WS-G1-ALPHA2.
           INITIALIZE WS-GROUP1
               REPLACING NUMERIC DATA BY 42.
           IF WS-G1-ALPHA = SPACES
               AND WS-G1-NUM = 42
               AND WS-G1-ALPHA2 = SPACES
               DISPLAY "NC287A-TEST-1 PASS"
           ELSE
               DISPLAY "NC287A-TEST-1 FAIL"
               DISPLAY "  ALPHA=>" WS-G1-ALPHA "<"
               DISPLAY "  NUM=" WS-G1-NUM
               DISPLAY "  ALPHA2=>" WS-G1-ALPHA2 "<"
           END-IF.
      * Test 2: INITIALIZE REPLACING ALPHANUMERIC DATA BY
      *   Alpha fields get "*", numeric fields get default 0
           MOVE "TEST" TO WS-G2-ALPHA.
           MOVE 111 TO WS-G2-NUM1.
           MOVE 222 TO WS-G2-NUM2.
           INITIALIZE WS-GROUP2
               REPLACING ALPHANUMERIC DATA BY "*".
           IF WS-G2-ALPHA(1:1) = "*"
               AND WS-G2-NUM1 = 0
               AND WS-G2-NUM2 = 0
               DISPLAY "NC287A-TEST-2 PASS"
           ELSE
               DISPLAY "NC287A-TEST-2 FAIL"
               DISPLAY "  ALPHA=>" WS-G2-ALPHA "<"
               DISPLAY "  NUM1=" WS-G2-NUM1
               DISPLAY "  NUM2=" WS-G2-NUM2
           END-IF.
      * Test 3: Plain INITIALIZE (no REPLACING)
      *   All alpha fields get spaces, all numeric fields get 0
           MOVE "ABCDEF" TO WS-G3-ALPHA.
           MOVE 5555 TO WS-G3-NUM.
           INITIALIZE WS-GROUP3.
           IF WS-G3-ALPHA = SPACES
               AND WS-G3-NUM = 0
               DISPLAY "NC287A-TEST-3 PASS"
           ELSE
               DISPLAY "NC287A-TEST-3 FAIL"
               DISPLAY "  ALPHA=>" WS-G3-ALPHA "<"
               DISPLAY "  NUM=" WS-G3-NUM
           END-IF.
           STOP RUN.
