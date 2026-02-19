       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC229A.
      *
      * NIST CCVS-style test: INITIALIZE REPLACING ALPHANUMERIC BY
      * and NUMERIC BY. Tests that INITIALIZE with REPLACING
      * clauses sets specific category fields to the given value
      * while leaving the other category at default.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP1.
           05 WS-G1-ALPHA     PIC X(5) VALUE "HELLO".
           05 WS-G1-NUM       PIC 9(4) VALUE 1234.
           05 WS-G1-ALPHA2    PIC X(3) VALUE "XYZ".
       01 WS-GROUP2.
           05 WS-G2-ALPHA     PIC X(4) VALUE "ABCD".
           05 WS-G2-NUM       PIC 9(3) VALUE 999.
       PROCEDURE DIVISION.
      * Test 1: INITIALIZE REPLACING ALPHANUMERIC BY literal
      *   Alpha fields get "ZZZZZ"/"ZZZ", numeric gets default 0
           MOVE "HELLO" TO WS-G1-ALPHA.
           MOVE 1234 TO WS-G1-NUM.
           MOVE "XYZ" TO WS-G1-ALPHA2.
           INITIALIZE WS-GROUP1
               REPLACING ALPHANUMERIC DATA BY "Z".
           IF WS-G1-ALPHA = "Z    "
               AND WS-G1-NUM = 0
               AND WS-G1-ALPHA2 = "Z  "
               DISPLAY "NC229A-TEST-1 PASS"
           ELSE
               DISPLAY "NC229A-TEST-1 FAIL"
               DISPLAY "  A1=>" WS-G1-ALPHA "<"
               DISPLAY "  NUM=" WS-G1-NUM
               DISPLAY "  A2=>" WS-G1-ALPHA2 "<"
           END-IF.
      * Test 2: INITIALIZE REPLACING NUMERIC BY literal
      *   Numeric fields get 42, alpha fields get default spaces
           MOVE "ABCD" TO WS-G2-ALPHA.
           MOVE 999 TO WS-G2-NUM.
           INITIALIZE WS-GROUP2
               REPLACING NUMERIC DATA BY 42.
           IF WS-G2-ALPHA = SPACES
               AND WS-G2-NUM = 42
               DISPLAY "NC229A-TEST-2 PASS"
           ELSE
               DISPLAY "NC229A-TEST-2 FAIL"
               DISPLAY "  ALPHA=>" WS-G2-ALPHA "<"
               DISPLAY "  NUM=" WS-G2-NUM
           END-IF.
           STOP RUN.
