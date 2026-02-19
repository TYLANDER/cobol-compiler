       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC205A.
      *
      * NIST CCVS-style test: Nested EVALUATE inside IF inside PERFORM
      * Tests complex nesting of control structures: PERFORM VARYING
      * containing IF/ELSE containing EVALUATE TRUE.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I            PIC 9(4) VALUE ZEROS.
       01 WS-SMALL-CT     PIC 9(4) VALUE ZEROS.
       01 WS-MED-CT       PIC 9(4) VALUE ZEROS.
       01 WS-LARGE-CT     PIC 9(4) VALUE ZEROS.
       01 WS-ODD-CT       PIC 9(4) VALUE ZEROS.
       01 WS-EVEN-CT      PIC 9(4) VALUE ZEROS.
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       01 WS-TOTAL        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: PERFORM VARYING with nested EVALUATE TRUE
      *   Loop 1..9, classify: <4 small, <7 medium, else large
      *   small=3 (1,2,3), medium=3 (4,5,6), large=3 (7,8,9)
           MOVE 0 TO WS-SMALL-CT WS-MED-CT WS-LARGE-CT.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 9
               EVALUATE TRUE
                   WHEN WS-I < 4
                       ADD 1 TO WS-SMALL-CT
                   WHEN WS-I < 7
                       ADD 1 TO WS-MED-CT
                   WHEN OTHER
                       ADD 1 TO WS-LARGE-CT
               END-EVALUATE
           END-PERFORM.
           IF WS-SMALL-CT = 3
               AND WS-MED-CT = 3
               AND WS-LARGE-CT = 3
               DISPLAY "NC205A-TEST-1 PASS"
           ELSE
               DISPLAY "NC205A-TEST-1 FAIL"
               DISPLAY "  S=" WS-SMALL-CT
                   " M=" WS-MED-CT
                   " L=" WS-LARGE-CT
           END-IF.
      * Test 2: PERFORM with IF containing nested EVALUATE
      *   Loop 1..10, if odd evaluate range, count categories
           MOVE 0 TO WS-ODD-CT WS-EVEN-CT WS-TOTAL.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 10
               IF WS-I = 1 OR WS-I = 3 OR WS-I = 5
                   OR WS-I = 7 OR WS-I = 9
                   ADD 1 TO WS-ODD-CT
                   EVALUATE TRUE
                       WHEN WS-I < 5
                           ADD 1 TO WS-TOTAL
                       WHEN WS-I >= 5
                           ADD 2 TO WS-TOTAL
                   END-EVALUATE
               ELSE
                   ADD 1 TO WS-EVEN-CT
               END-IF
           END-PERFORM.
      *   Odd: 1,3,5,7,9 (5 items), Even: 2,4,6,8,10 (5 items)
      *   Total: 1<5(+1), 3<5(+1), 5>=5(+2), 7>=5(+2), 9>=5(+2) = 8
           IF WS-ODD-CT = 5
               AND WS-EVEN-CT = 5
               AND WS-TOTAL = 8
               DISPLAY "NC205A-TEST-2 PASS"
           ELSE
               DISPLAY "NC205A-TEST-2 FAIL"
               DISPLAY "  ODD=" WS-ODD-CT
                   " EVEN=" WS-EVEN-CT
                   " TOTAL=" WS-TOTAL
           END-IF.
           STOP RUN.
