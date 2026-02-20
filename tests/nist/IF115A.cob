       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF115A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FIELD            PIC X(10) VALUE "  hello   ".
       01  WS-RESULT           PIC X(10).
       01  WS-LEN1             PIC 9(4).
       01  WS-LEN2             PIC 9(4).
       01  WS-FIELD2           PIC X(5) VALUE "ABCDE".
       PROCEDURE DIVISION.
      *    TEST 1: UPPER-CASE(TRIM(field)) — nested calls
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(WS-FIELD))
               TO WS-RESULT
           IF WS-RESULT = "HELLO     "
               DISPLAY "IF115A-TEST-1 PASS"
           ELSE
               DISPLAY "IF115A-TEST-1 FAIL"
           END-IF.
      *    TEST 2: LENGTH(REVERSE(field)) = LENGTH(field)
           COMPUTE WS-LEN1 =
               FUNCTION LENGTH(
               FUNCTION REVERSE(WS-FIELD2))
           COMPUTE WS-LEN2 =
               FUNCTION LENGTH(WS-FIELD2)
           IF WS-LEN1 = WS-LEN2
               DISPLAY "IF115A-TEST-2 PASS"
           ELSE
               DISPLAY "IF115A-TEST-2 FAIL"
           END-IF.
           STOP RUN.
