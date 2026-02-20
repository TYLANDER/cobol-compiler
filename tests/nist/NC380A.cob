       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC380A.
      *
      * NIST CCVS-style test: Complex PERFORM VARYING with
      * AFTER clause (nested loop).
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I            PIC 9(3) VALUE 0.
       01 WS-J            PIC 9(3) VALUE 0.
       01 WS-COUNT        PIC 9(3) VALUE 0.
       01 WS-K            PIC 9(3) VALUE 0.
       01 WS-L            PIC 9(3) VALUE 0.
       01 WS-COUNT2       PIC 9(3) VALUE 0.
       01 WS-M            PIC 9(3) VALUE 0.
       01 WS-N            PIC 9(3) VALUE 0.
       01 WS-P            PIC 9(3) VALUE 0.
       01 WS-COUNT3       PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: VARYING I 1-3, AFTER J 1-2 => 6 iterations
           MOVE 0 TO WS-COUNT.
           PERFORM NC380A-PARA-1
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               AFTER WS-J FROM 1 BY 1 UNTIL WS-J > 2.
           IF WS-COUNT = 6
               DISPLAY "NC380A-TEST-1 PASS"
           ELSE
               DISPLAY "NC380A-TEST-1 FAIL"
               DISPLAY "  Expected 6, got " WS-COUNT
           END-IF.
      * Test 2: VARYING K 1-4, AFTER L 1-3 => 12 iterations
           MOVE 0 TO WS-COUNT2.
           PERFORM NC380A-PARA-2
               VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 4
               AFTER WS-L FROM 1 BY 1 UNTIL WS-L > 3.
           IF WS-COUNT2 = 12
               DISPLAY "NC380A-TEST-2 PASS"
           ELSE
               DISPLAY "NC380A-TEST-2 FAIL"
               DISPLAY "  Expected 12, got " WS-COUNT2
           END-IF.
      * Test 3: Triple nested VARYING/AFTER/AFTER => 8
           MOVE 0 TO WS-COUNT3.
           PERFORM NC380A-PARA-3
               VARYING WS-M FROM 1 BY 1 UNTIL WS-M > 2
               AFTER WS-N FROM 1 BY 1 UNTIL WS-N > 2
               AFTER WS-P FROM 1 BY 1 UNTIL WS-P > 2.
           IF WS-COUNT3 = 8
               DISPLAY "NC380A-TEST-3 PASS"
           ELSE
               DISPLAY "NC380A-TEST-3 FAIL"
               DISPLAY "  Expected 8, got " WS-COUNT3
           END-IF.
           STOP RUN.
       NC380A-PARA-1.
           ADD 1 TO WS-COUNT.
       NC380A-PARA-2.
           ADD 1 TO WS-COUNT2.
       NC380A-PARA-3.
           ADD 1 TO WS-COUNT3.
