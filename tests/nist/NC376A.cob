       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC376A.
      *
      * NIST CCVS-style test: Arithmetic overflow with
      * ON SIZE ERROR.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A            PIC 9(3)  VALUE 999.
       01 WS-B            PIC 9(3)  VALUE 1.
       01 WS-C            PIC 9(3)  VALUE 0.
       01 WS-FLAG          PIC 9    VALUE 0.
       01 WS-D            PIC 9(3)  VALUE 500.
       01 WS-E            PIC 9(3)  VALUE 600.
       01 WS-F            PIC 9(3)  VALUE 0.
       01 WS-FLAG2         PIC 9    VALUE 0.
       01 WS-G            PIC 9(3)  VALUE 100.
       01 WS-H            PIC 9(3)  VALUE 50.
       01 WS-I            PIC 9(3)  VALUE 0.
       01 WS-FLAG3         PIC 9    VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: ADD 999 + 1 => 1000 overflows PIC 9(3)
           MOVE 0 TO WS-FLAG.
           MOVE 0 TO WS-C.
           ADD WS-A WS-B GIVING WS-C
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG
               NOT ON SIZE ERROR
                   MOVE 0 TO WS-FLAG
           END-ADD.
           IF WS-FLAG = 1
               DISPLAY "NC376A-TEST-1 PASS"
           ELSE
               DISPLAY "NC376A-TEST-1 FAIL"
               DISPLAY "  SIZE ERROR not triggered, C=" WS-C
           END-IF.
      * Test 2: ADD 500 + 600 => 1100 overflows PIC 9(3)
           MOVE 0 TO WS-FLAG2.
           MOVE 0 TO WS-F.
           ADD WS-D WS-E GIVING WS-F
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG2
               NOT ON SIZE ERROR
                   MOVE 0 TO WS-FLAG2
           END-ADD.
           IF WS-FLAG2 = 1
               DISPLAY "NC376A-TEST-2 PASS"
           ELSE
               DISPLAY "NC376A-TEST-2 FAIL"
               DISPLAY "  SIZE ERROR not triggered, F=" WS-F
           END-IF.
      * Test 3: ADD 100 + 50 = 150 does NOT overflow
           MOVE 0 TO WS-FLAG3.
           MOVE 0 TO WS-I.
           ADD WS-G WS-H GIVING WS-I
               ON SIZE ERROR
                   MOVE 1 TO WS-FLAG3
               NOT ON SIZE ERROR
                   MOVE 2 TO WS-FLAG3
           END-ADD.
           IF WS-FLAG3 = 2 AND WS-I = 150
               DISPLAY "NC376A-TEST-3 PASS"
           ELSE
               DISPLAY "NC376A-TEST-3 FAIL"
               DISPLAY "  FLAG3=" WS-FLAG3 " I=" WS-I
           END-IF.
           STOP RUN.
