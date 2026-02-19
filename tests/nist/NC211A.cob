       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC211A.
      *
      * NIST CCVS-style test: PERFORM UNTIL with complex exit
      * conditions using compound conditions (AND/OR) in the
      * UNTIL clause.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I            PIC 9(4) VALUE ZEROS.
       01 WS-SUM          PIC 9(4) VALUE ZEROS.
       01 WS-FLAG         PIC 9    VALUE 0.
       01 WS-COUNT        PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
      * Test 1: PERFORM UNTIL with AND condition
      *   Loop while WS-I < 10 AND WS-SUM < 20
      *   i=1 sum=1, i=2 sum=3, i=3 sum=6, i=4 sum=10,
      *   i=5 sum=15, i=6 sum=21 -> exits because sum >= 20
      *   Final: i=6, sum=21
           MOVE 0 TO WS-I.
           MOVE 0 TO WS-SUM.
           PERFORM UNTIL WS-I >= 10 OR WS-SUM >= 20
               ADD 1 TO WS-I
               ADD WS-I TO WS-SUM
           END-PERFORM.
           IF WS-I = 6 AND WS-SUM = 21
               DISPLAY "NC211A-TEST-1 PASS"
           ELSE
               DISPLAY "NC211A-TEST-1 FAIL"
               DISPLAY "  Expected I=6 SUM=21, got I="
                   WS-I " SUM=" WS-SUM
           END-IF.
      * Test 2: PERFORM UNTIL with OR condition
      *   Loop until flag=1 OR count>5
      *   Set flag=1 when count reaches 3
      *   c=1, c=2, c=3 flag=1 -> exits
      *   Final: count=3, flag=1
           MOVE 0 TO WS-COUNT.
           MOVE 0 TO WS-FLAG.
           PERFORM UNTIL WS-FLAG = 1 OR WS-COUNT > 5
               ADD 1 TO WS-COUNT
               IF WS-COUNT = 3
                   MOVE 1 TO WS-FLAG
               END-IF
           END-PERFORM.
           IF WS-COUNT = 3 AND WS-FLAG = 1
               DISPLAY "NC211A-TEST-2 PASS"
           ELSE
               DISPLAY "NC211A-TEST-2 FAIL"
               DISPLAY "  Expected COUNT=3 FLAG=1, got COUNT="
                   WS-COUNT " FLAG=" WS-FLAG
           END-IF.
      * Test 3: PERFORM UNTIL with AND condition (both must be true)
      *   Loop until i > 3 AND sum > 5
      *   i=1 s=1, i=2 s=3, i=3 s=6, i=4 s=10
      *   At i=4: i>3 is true AND sum(10)>5 is true -> exits
      *   Final: i=4, sum=10
           MOVE 0 TO WS-I.
           MOVE 0 TO WS-SUM.
           PERFORM UNTIL WS-I > 3 AND WS-SUM > 5
               ADD 1 TO WS-I
               ADD WS-I TO WS-SUM
           END-PERFORM.
           IF WS-I = 4 AND WS-SUM = 10
               DISPLAY "NC211A-TEST-3 PASS"
           ELSE
               DISPLAY "NC211A-TEST-3 FAIL"
               DISPLAY "  Expected I=4 SUM=10, got I="
                   WS-I " SUM=" WS-SUM
           END-IF.
           STOP RUN.
