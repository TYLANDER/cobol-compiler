       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC370A.
      *
      * NIST CCVS-style test: GO TO DEPENDING ON with boundary
      * values. Test with values 0, 1, 2, 3, 4 where the
      * valid range is 1-3.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INDEX        PIC 9 VALUE 0.
       01 WS-RESULT       PIC X(10) VALUE SPACES.
       01 WS-TEST-NUM     PIC 9 VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: GO TO DEPENDING ON with value 2 (valid)
      * Should branch to PARA-2
           MOVE 1 TO WS-TEST-NUM.
           MOVE 2 TO WS-INDEX.
           GO TO PARA-1-T1 PARA-2-T1 PARA-3-T1
               DEPENDING ON WS-INDEX.
      * Fall-through means DEPENDING ON did not branch
           MOVE "FALLTHRU" TO WS-RESULT.
           GO TO TEST-1-CHECK.
       PARA-1-T1.
           MOVE "PARA-1" TO WS-RESULT.
           GO TO TEST-1-CHECK.
       PARA-2-T1.
           MOVE "PARA-2" TO WS-RESULT.
           GO TO TEST-1-CHECK.
       PARA-3-T1.
           MOVE "PARA-3" TO WS-RESULT.
           GO TO TEST-1-CHECK.
       TEST-1-CHECK.
           IF WS-RESULT = "PARA-2"
               DISPLAY "NC370A-TEST-1 PASS"
           ELSE
               DISPLAY "NC370A-TEST-1 FAIL"
               DISPLAY "  Expected PARA-2, got [" WS-RESULT "]"
           END-IF.
      * Test 2: GO TO DEPENDING ON with value 0 (below range)
      * Should fall through (no branch taken)
           MOVE 2 TO WS-TEST-NUM.
           MOVE 0 TO WS-INDEX.
           GO TO PARA-1-T2 PARA-2-T2 PARA-3-T2
               DEPENDING ON WS-INDEX.
      * Fall-through: value 0 is out of range 1-3
           MOVE "FALLTHRU" TO WS-RESULT.
           GO TO TEST-2-CHECK.
       PARA-1-T2.
           MOVE "PARA-1" TO WS-RESULT.
           GO TO TEST-2-CHECK.
       PARA-2-T2.
           MOVE "PARA-2" TO WS-RESULT.
           GO TO TEST-2-CHECK.
       PARA-3-T2.
           MOVE "PARA-3" TO WS-RESULT.
           GO TO TEST-2-CHECK.
       TEST-2-CHECK.
           IF WS-RESULT = "FALLTHRU"
               DISPLAY "NC370A-TEST-2 PASS"
           ELSE
               DISPLAY "NC370A-TEST-2 FAIL"
               DISPLAY "  Expected FALLTHRU, got [" WS-RESULT
                   "]"
           END-IF.
      * Test 3: GO TO DEPENDING ON with value 4 (above range)
      * Should fall through (no branch taken)
           MOVE 3 TO WS-TEST-NUM.
           MOVE 4 TO WS-INDEX.
           GO TO PARA-1-T3 PARA-2-T3 PARA-3-T3
               DEPENDING ON WS-INDEX.
      * Fall-through: value 4 is out of range 1-3
           MOVE "FALLTHRU" TO WS-RESULT.
           GO TO TEST-3-CHECK.
       PARA-1-T3.
           MOVE "PARA-1" TO WS-RESULT.
           GO TO TEST-3-CHECK.
       PARA-2-T3.
           MOVE "PARA-2" TO WS-RESULT.
           GO TO TEST-3-CHECK.
       PARA-3-T3.
           MOVE "PARA-3" TO WS-RESULT.
           GO TO TEST-3-CHECK.
       TEST-3-CHECK.
           IF WS-RESULT = "FALLTHRU"
               DISPLAY "NC370A-TEST-3 PASS"
           ELSE
               DISPLAY "NC370A-TEST-3 FAIL"
               DISPLAY "  Expected FALLTHRU, got [" WS-RESULT
                   "]"
           END-IF.
           STOP RUN.
