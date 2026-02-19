       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC197A.
      *
      * NIST CCVS-style test: INITIALIZE with REPLACING
      * Tests INITIALIZE REPLACING NUMERIC BY and REPLACING
      * ALPHANUMERIC BY to set specific default values by
      * data category within a group item.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-REC1.
           05 WS-R1-NAME   PIC X(8) VALUE "ORIGINAL".
           05 WS-R1-AMT    PIC 9(5) VALUE 99999.
           05 WS-R1-CODE   PIC X(3) VALUE "ZZZ".
           05 WS-R1-QTY    PIC 9(3) VALUE 888.
       01 WS-REC2.
           05 WS-R2-LABEL  PIC X(6) VALUE "ABCDEF".
           05 WS-R2-VAL1   PIC 9(4) VALUE 1111.
           05 WS-R2-TAG    PIC X(4) VALUE "WXYZ".
           05 WS-R2-VAL2   PIC 9(4) VALUE 2222.
       01 WS-REC3.
           05 WS-R3-DESC   PIC X(10) VALUE "DIRTY DATA".
           05 WS-R3-PRICE  PIC 9(4)  VALUE 5000.
       PROCEDURE DIVISION.
      * Test 1: INITIALIZE REPLACING NUMERIC BY 77
      *   Numeric fields get 77, alpha fields get SPACES
           MOVE "ORIGINAL" TO WS-R1-NAME.
           MOVE 99999 TO WS-R1-AMT.
           MOVE "ZZZ" TO WS-R1-CODE.
           MOVE 888 TO WS-R1-QTY.
           INITIALIZE WS-REC1
               REPLACING NUMERIC DATA BY 77.
           IF WS-R1-NAME = SPACES
               AND WS-R1-AMT = 77
               AND WS-R1-CODE = SPACES
               AND WS-R1-QTY = 77
               DISPLAY "NC197A-TEST-1 PASS"
           ELSE
               DISPLAY "NC197A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-R1-NAME "<"
               DISPLAY "  AMT=" WS-R1-AMT
               DISPLAY "  CODE=>" WS-R1-CODE "<"
               DISPLAY "  QTY=" WS-R1-QTY
           END-IF.
      * Test 2: INITIALIZE REPLACING ALPHANUMERIC BY
      *   Alphanumeric fields get replacement, numeric get ZEROS
           MOVE "ABCDEF" TO WS-R2-LABEL.
           MOVE 1111 TO WS-R2-VAL1.
           MOVE "WXYZ" TO WS-R2-TAG.
           MOVE 2222 TO WS-R2-VAL2.
           INITIALIZE WS-REC2
               REPLACING ALPHANUMERIC DATA BY "*".
           IF WS-R2-LABEL(1:1) = "*"
               AND WS-R2-VAL1 = ZEROS
               AND WS-R2-TAG(1:1) = "*"
               AND WS-R2-VAL2 = ZEROS
               DISPLAY "NC197A-TEST-2 PASS"
           ELSE
               DISPLAY "NC197A-TEST-2 FAIL"
               DISPLAY "  LABEL=>" WS-R2-LABEL "<"
               DISPLAY "  VAL1=" WS-R2-VAL1
               DISPLAY "  TAG=>" WS-R2-TAG "<"
               DISPLAY "  VAL2=" WS-R2-VAL2
           END-IF.
      * Test 3: Plain INITIALIZE (no REPLACING) resets defaults
      *   Alphanumeric -> SPACES, Numeric -> ZEROS
           MOVE "DIRTY DATA" TO WS-R3-DESC.
           MOVE 5000 TO WS-R3-PRICE.
           INITIALIZE WS-REC3.
           IF WS-R3-DESC = SPACES AND WS-R3-PRICE = ZEROS
               DISPLAY "NC197A-TEST-3 PASS"
           ELSE
               DISPLAY "NC197A-TEST-3 FAIL"
               DISPLAY "  DESC=>" WS-R3-DESC "<"
               DISPLAY "  PRICE=" WS-R3-PRICE
           END-IF.
           STOP RUN.
