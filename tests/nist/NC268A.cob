       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC268A.
      *
      * NIST CCVS-style test: Multiple GIVING targets
      * Tests ADD/MULTIPLY/SUBTRACT with GIVING storing result
      * in multiple target identifiers.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A              PIC 9(4)  VALUE ZEROS.
       01 WS-B              PIC 9(4)  VALUE ZEROS.
       01 WS-C              PIC 9(4)  VALUE ZEROS.
       01 WS-D              PIC 9(4)  VALUE ZEROS.
       01 WS-E              PIC 9(4)  VALUE ZEROS.
       01 WS-F              PIC 9(4)  VALUE ZEROS.
       01 WS-G              PIC 9(4)  VALUE ZEROS.
       01 WS-H              PIC 9(4)  VALUE ZEROS.
       01 WS-X              PIC 9(4)  VALUE ZEROS.
       01 WS-Y              PIC 9(4)  VALUE ZEROS.
       01 WS-R1             PIC 9(4)  VALUE ZEROS.
       01 WS-R2             PIC 9(4)  VALUE ZEROS.
       01 WS-R3             PIC 9(4)  VALUE ZEROS.
       PROCEDURE DIVISION.
       NC268A-CONTROL.
           PERFORM NC268A-TEST-1
           PERFORM NC268A-TEST-2
           PERFORM NC268A-TEST-3
           STOP RUN.
       NC268A-TEST-1.
      * ADD A TO B GIVING C D E
      *   A=100 B=250 => 100+250=350 stored in C, D, E
           MOVE 100 TO WS-A.
           MOVE 250 TO WS-B.
           MOVE 0 TO WS-C.
           MOVE 0 TO WS-D.
           MOVE 0 TO WS-E.
           ADD WS-A TO WS-B GIVING WS-C WS-D WS-E.
           IF WS-C = 350
               AND WS-D = 350
               AND WS-E = 350
               DISPLAY "NC268A-TEST-1 PASS"
           ELSE
               DISPLAY "NC268A-TEST-1 FAIL"
               DISPLAY "  C=" WS-C " D=" WS-D " E=" WS-E
           END-IF.
       NC268A-TEST-2.
      * MULTIPLY A BY B GIVING F G H
      *   A=12 B=5 => 12*5=60 stored in F, G, H
           MOVE 12 TO WS-A.
           MOVE 5  TO WS-B.
           MOVE 0 TO WS-F.
           MOVE 0 TO WS-G.
           MOVE 0 TO WS-H.
           MULTIPLY WS-A BY WS-B GIVING WS-F WS-G WS-H.
           IF WS-F = 60
               AND WS-G = 60
               AND WS-H = 60
               DISPLAY "NC268A-TEST-2 PASS"
           ELSE
               DISPLAY "NC268A-TEST-2 FAIL"
               DISPLAY "  F=" WS-F " G=" WS-G " H=" WS-H
           END-IF.
       NC268A-TEST-3.
      * SUBTRACT X FROM Y GIVING R1 R2 R3
      *   X=30 Y=100 => 100-30=70 stored in R1, R2, R3
           MOVE 30 TO WS-X.
           MOVE 100 TO WS-Y.
           MOVE 0 TO WS-R1.
           MOVE 0 TO WS-R2.
           MOVE 0 TO WS-R3.
           SUBTRACT WS-X FROM WS-Y GIVING WS-R1 WS-R2 WS-R3.
           IF WS-R1 = 70
               AND WS-R2 = 70
               AND WS-R3 = 70
               DISPLAY "NC268A-TEST-3 PASS"
           ELSE
               DISPLAY "NC268A-TEST-3 FAIL"
               DISPLAY "  R1=" WS-R1 " R2=" WS-R2 " R3=" WS-R3
           END-IF.
