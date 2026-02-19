       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC265A.
      *
      * NIST CCVS-style test: GO TO DEPENDING ON
      * Tests GO TO ... DEPENDING ON with multiple paragraph
      * targets.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INDEX           PIC 9(1)  VALUE ZEROS.
       01 WS-RESULT1         PIC X(10) VALUE SPACES.
       01 WS-RESULT2         PIC X(10) VALUE SPACES.
       01 WS-RESULT3         PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
       NC265A-CONTROL.
           PERFORM NC265A-TEST-1
           PERFORM NC265A-TEST-2
           PERFORM NC265A-TEST-3
           STOP RUN.
       NC265A-TEST-1.
      * GO TO DEPENDING ON with index=2 => second paragraph
           MOVE 2 TO WS-INDEX.
           GO TO NC265A-T1-P1
                 NC265A-T1-P2
                 NC265A-T1-P3
               DEPENDING ON WS-INDEX.
           MOVE "FALL-THRU" TO WS-RESULT1.
           GO TO NC265A-T1-CHECK.
       NC265A-T1-P1.
           MOVE "FIRST" TO WS-RESULT1.
           GO TO NC265A-T1-CHECK.
       NC265A-T1-P2.
           MOVE "SECOND" TO WS-RESULT1.
           GO TO NC265A-T1-CHECK.
       NC265A-T1-P3.
           MOVE "THIRD" TO WS-RESULT1.
           GO TO NC265A-T1-CHECK.
       NC265A-T1-CHECK.
           IF WS-RESULT1 = "SECOND"
               DISPLAY "NC265A-TEST-1 PASS"
           ELSE
               DISPLAY "NC265A-TEST-1 FAIL"
               DISPLAY "  RESULT1=" WS-RESULT1
           END-IF.
       NC265A-TEST-2.
      * GO TO DEPENDING ON with index=1 => first paragraph
           MOVE 1 TO WS-INDEX.
           GO TO NC265A-T2-P1
                 NC265A-T2-P2
                 NC265A-T2-P3
               DEPENDING ON WS-INDEX.
           MOVE "FALL-THRU" TO WS-RESULT2.
           GO TO NC265A-T2-CHECK.
       NC265A-T2-P1.
           MOVE "FIRST" TO WS-RESULT2.
           GO TO NC265A-T2-CHECK.
       NC265A-T2-P2.
           MOVE "SECOND" TO WS-RESULT2.
           GO TO NC265A-T2-CHECK.
       NC265A-T2-P3.
           MOVE "THIRD" TO WS-RESULT2.
           GO TO NC265A-T2-CHECK.
       NC265A-T2-CHECK.
           IF WS-RESULT2 = "FIRST"
               DISPLAY "NC265A-TEST-2 PASS"
           ELSE
               DISPLAY "NC265A-TEST-2 FAIL"
               DISPLAY "  RESULT2=" WS-RESULT2
           END-IF.
       NC265A-TEST-3.
      * GO TO DEPENDING ON with index=0 (out of range)
      *   Should fall through to next statement
           MOVE 0 TO WS-INDEX.
           GO TO NC265A-T3-P1
                 NC265A-T3-P2
               DEPENDING ON WS-INDEX.
           MOVE "FALL-THRU" TO WS-RESULT3.
           GO TO NC265A-T3-CHECK.
       NC265A-T3-P1.
           MOVE "FIRST" TO WS-RESULT3.
           GO TO NC265A-T3-CHECK.
       NC265A-T3-P2.
           MOVE "SECOND" TO WS-RESULT3.
           GO TO NC265A-T3-CHECK.
       NC265A-T3-CHECK.
           IF WS-RESULT3 = "FALL-THRU"
               DISPLAY "NC265A-TEST-3 PASS"
           ELSE
               DISPLAY "NC265A-TEST-3 FAIL"
               DISPLAY "  RESULT3=" WS-RESULT3
           END-IF.
