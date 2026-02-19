       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC103A.
      *
      * NIST CCVS-style test: IF conditions and PERFORM
      * Tests conditional logic and loops.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VALUE PIC 9(2) VALUE ZEROS.
       01 WS-SUM PIC 9(4) VALUE ZEROS.
       01 WS-I PIC 9(2) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: Simple IF greater-than
           MOVE 42 TO WS-VALUE.
           IF WS-VALUE > 40
               DISPLAY "NC103A-TEST-1 PASS"
           ELSE
               DISPLAY "NC103A-TEST-1 FAIL"
           END-IF.
      * Test 2: IF less-than
           MOVE 15 TO WS-VALUE.
           IF WS-VALUE < 20
               DISPLAY "NC103A-TEST-2 PASS"
           ELSE
               DISPLAY "NC103A-TEST-2 FAIL"
           END-IF.
      * Test 3: PERFORM loop computing sum 1..10
           MOVE 0 TO WS-SUM.
           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > 10
               ADD WS-I TO WS-SUM
           END-PERFORM.
           IF WS-SUM = 55
               DISPLAY "NC103A-TEST-3 PASS"
           ELSE
               DISPLAY "NC103A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
