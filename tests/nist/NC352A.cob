       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC352A.
      *
      * NIST CCVS-style test: STRING with multiple source fields
      * and DELIMITED BY SIZE.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRC-1        PIC X(5) VALUE "HELLO".
       01 WS-SRC-2        PIC X(5) VALUE "WORLD".
       01 WS-SRC-3        PIC X(3) VALUE "ABC".
       01 WS-DEST         PIC X(20) VALUE SPACES.
       01 WS-PTR          PIC 99 VALUE 1.
       PROCEDURE DIVISION.
      * Test 1: STRING two fields DELIMITED BY SIZE
      * "HELLO" + "WORLD" => "HELLOWORLD          "
           MOVE SPACES TO WS-DEST.
           MOVE 1 TO WS-PTR.
           STRING WS-SRC-1 DELIMITED BY SIZE
                  WS-SRC-2 DELIMITED BY SIZE
                  INTO WS-DEST WITH POINTER WS-PTR.
           IF WS-DEST = "HELLOWORLD          "
               DISPLAY "NC352A-TEST-1 PASS"
           ELSE
               DISPLAY "NC352A-TEST-1 FAIL"
               DISPLAY "  Expected HELLOWORLD"
               DISPLAY "  Got      " WS-DEST
           END-IF.
      * Test 2: Verify POINTER value after STRING
      * Pointer should be 11 (1 + 5 + 5)
           IF WS-PTR = 11
               DISPLAY "NC352A-TEST-2 PASS"
           ELSE
               DISPLAY "NC352A-TEST-2 FAIL"
               DISPLAY "  Expected pointer 11, got " WS-PTR
           END-IF.
      * Test 3: STRING three fields with pointer starting at 1
           MOVE SPACES TO WS-DEST.
           MOVE 1 TO WS-PTR.
           STRING WS-SRC-1 DELIMITED BY SIZE
                  WS-SRC-2 DELIMITED BY SIZE
                  WS-SRC-3 DELIMITED BY SIZE
                  INTO WS-DEST WITH POINTER WS-PTR.
           IF WS-DEST = "HELLOWORLDABC       "
               DISPLAY "NC352A-TEST-3 PASS"
           ELSE
               DISPLAY "NC352A-TEST-3 FAIL"
               DISPLAY "  Expected HELLOWORLDABC"
               DISPLAY "  Got      " WS-DEST
           END-IF.
           STOP RUN.
