       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC210A.
      *
      * NIST CCVS-style test: CALL with literal name and large param
      * Tests using a literal string program name with a large
      * alphanumeric parameter to verify correct data transfer.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-BIG    PIC X(30) VALUE SPACES.
       01 WS-FLAG   PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: CALL literal sets large alphanumeric parameter
           CALL "IC210A-SUB" USING WS-BIG WS-FLAG.
           IF WS-BIG = "LARGE-PARAMETER-VALUE-RECEIVED"
               DISPLAY "IC210A-TEST-1 PASS"
           ELSE
               DISPLAY "IC210A-TEST-1 FAIL"
               DISPLAY "  BIG=>" WS-BIG "<"
           END-IF.
      * Test 2: Flag confirms subprogram executed
           IF WS-FLAG = 1
               DISPLAY "IC210A-TEST-2 PASS"
           ELSE
               DISPLAY "IC210A-TEST-2 FAIL"
               DISPLAY "  FLAG=" WS-FLAG
           END-IF.
      * Test 3: Second CALL with literal resets and works again
           MOVE SPACES TO WS-BIG.
           MOVE 0 TO WS-FLAG.
           CALL "IC210A-SUB" USING WS-BIG WS-FLAG.
           IF WS-BIG = "LARGE-PARAMETER-VALUE-RECEIVED"
             AND WS-FLAG = 1
               DISPLAY "IC210A-TEST-3 PASS"
           ELSE
               DISPLAY "IC210A-TEST-3 FAIL"
           END-IF.
           STOP RUN.
