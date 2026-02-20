       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC379A.
      *
      * NIST CCVS-style test: INITIALIZE with REPLACING
      * phrase.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP-1.
           05 WS-G1-ALPHA PIC X(5) VALUE "HELLO".
           05 WS-G1-NUM   PIC 9(3) VALUE 123.
           05 WS-G1-ALP2  PIC X(4) VALUE "TEST".
       01 WS-GROUP-2.
           05 WS-G2-NUM1  PIC 9(3) VALUE 100.
           05 WS-G2-ALPHA PIC X(3) VALUE "ABC".
           05 WS-G2-NUM2  PIC 9(3) VALUE 200.
       01 WS-GROUP-3.
           05 WS-G3-ALPHA PIC X(6) VALUE "ABCDEF".
           05 WS-G3-NUM   PIC 9(4) VALUE 9999.
       PROCEDURE DIVISION.
      * Test 1: INITIALIZE REPLACING NUMERIC BY 5
           INITIALIZE WS-GROUP-1
               REPLACING NUMERIC DATA BY 5.
           IF WS-G1-ALPHA = SPACES AND WS-G1-NUM = 5
               AND WS-G1-ALP2 = SPACES
               DISPLAY "NC379A-TEST-1 PASS"
           ELSE
               DISPLAY "NC379A-TEST-1 FAIL"
               DISPLAY "  ALPHA=" WS-G1-ALPHA
               DISPLAY "  NUM=" WS-G1-NUM
               DISPLAY "  ALP2=" WS-G1-ALP2
           END-IF.
      * Test 2: INITIALIZE REPLACING ALPHANUMERIC BY "X"
           INITIALIZE WS-GROUP-2
               REPLACING ALPHANUMERIC DATA BY "X".
           IF WS-G2-ALPHA = "X  " AND WS-G2-NUM1 = 0
               AND WS-G2-NUM2 = 0
               DISPLAY "NC379A-TEST-2 PASS"
           ELSE
               DISPLAY "NC379A-TEST-2 FAIL"
               DISPLAY "  NUM1=" WS-G2-NUM1
               DISPLAY "  ALPHA=" WS-G2-ALPHA
               DISPLAY "  NUM2=" WS-G2-NUM2
           END-IF.
      * Test 3: INITIALIZE REPLACING both types
           MOVE "ABCDEF" TO WS-G3-ALPHA.
           MOVE 9999 TO WS-G3-NUM.
           INITIALIZE WS-GROUP-3
               REPLACING ALPHANUMERIC DATA BY "Z"
               NUMERIC DATA BY 7.
           IF WS-G3-ALPHA = "Z     " AND WS-G3-NUM = 7
               DISPLAY "NC379A-TEST-3 PASS"
           ELSE
               DISPLAY "NC379A-TEST-3 FAIL"
               DISPLAY "  ALPHA=" WS-G3-ALPHA
               DISPLAY "  NUM=" WS-G3-NUM
           END-IF.
           STOP RUN.
