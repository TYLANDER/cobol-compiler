       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC348A.
      *
      * NIST CCVS-style test: INITIALIZE with REPLACING
      * Tests INITIALIZE REPLACING NUMERIC BY and
      * REPLACING ALPHANUMERIC BY on group items.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-REC1.
           05 WS-R1-NAME     PIC X(8) VALUE "ORIGINAL".
           05 WS-R1-QTY      PIC 9(4) VALUE 9999.
           05 WS-R1-CODE     PIC X(3) VALUE "ABC".
       01 WS-REC2.
           05 WS-R2-DESC     PIC X(10) VALUE "DIRTY-DATA".
           05 WS-R2-AMT1     PIC 9(4) VALUE 1111.
           05 WS-R2-AMT2     PIC 9(4) VALUE 2222.
       01 WS-REC3.
           05 WS-R3-TAG      PIC X(5) VALUE "HELLO".
           05 WS-R3-VAL      PIC 9(6) VALUE 999999.
       PROCEDURE DIVISION.
      * Test 1: INITIALIZE REPLACING NUMERIC DATA BY literal
      *   Numeric fields get 42, alpha fields get default spaces
           MOVE "ORIGINAL" TO WS-R1-NAME.
           MOVE 9999 TO WS-R1-QTY.
           MOVE "ABC" TO WS-R1-CODE.
           INITIALIZE WS-REC1
               REPLACING NUMERIC DATA BY 42.
           IF WS-R1-NAME = SPACES
               AND WS-R1-QTY = 42
               AND WS-R1-CODE = SPACES
               DISPLAY "NC348A-TEST-1 PASS"
           ELSE
               DISPLAY "NC348A-TEST-1 FAIL"
               DISPLAY "  NAME=>" WS-R1-NAME "<"
               DISPLAY "  QTY=" WS-R1-QTY
               DISPLAY "  CODE=>" WS-R1-CODE "<"
           END-IF.
      * Test 2: INITIALIZE REPLACING ALPHANUMERIC DATA BY
      *   Alpha fields get "*", numeric fields get default 0
           MOVE "DIRTY-DATA" TO WS-R2-DESC.
           MOVE 1111 TO WS-R2-AMT1.
           MOVE 2222 TO WS-R2-AMT2.
           INITIALIZE WS-REC2
               REPLACING ALPHANUMERIC DATA BY "*".
           IF WS-R2-DESC(1:1) = "*"
               AND WS-R2-AMT1 = 0
               AND WS-R2-AMT2 = 0
               DISPLAY "NC348A-TEST-2 PASS"
           ELSE
               DISPLAY "NC348A-TEST-2 FAIL"
               DISPLAY "  DESC=>" WS-R2-DESC "<"
               DISPLAY "  AMT1=" WS-R2-AMT1
               DISPLAY "  AMT2=" WS-R2-AMT2
           END-IF.
      * Test 3: Plain INITIALIZE (no REPLACING)
      *   Alpha gets spaces, numeric gets zeros
           MOVE "HELLO" TO WS-R3-TAG.
           MOVE 999999 TO WS-R3-VAL.
           INITIALIZE WS-REC3.
           IF WS-R3-TAG = SPACES AND WS-R3-VAL = 0
               DISPLAY "NC348A-TEST-3 PASS"
           ELSE
               DISPLAY "NC348A-TEST-3 FAIL"
               DISPLAY "  TAG=>" WS-R3-TAG "<"
               DISPLAY "  VAL=" WS-R3-VAL
           END-IF.
           STOP RUN.
