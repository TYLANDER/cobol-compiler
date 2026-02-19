       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC261A.
      *
      * NIST CCVS-style test: MOVE CORRESPONDING between groups
      * with differently-ordered fields.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP-SRC.
           05 WS-CITY        PIC X(10) VALUE "DENVER".
           05 WS-STATE       PIC X(5)  VALUE "CO".
           05 WS-ZIP         PIC 9(5)  VALUE 80202.
       01 WS-GROUP-DST.
           05 WS-ZIP         PIC 9(5)  VALUE ZEROS.
           05 WS-CITY        PIC X(10) VALUE SPACES.
           05 WS-STATE       PIC X(5)  VALUE SPACES.
       01 WS-SRC2.
           05 WS-X           PIC 9(3) VALUE 100.
           05 WS-Y           PIC 9(3) VALUE 200.
           05 WS-Z           PIC 9(3) VALUE 300.
       01 WS-DST2.
           05 WS-Z           PIC 9(3) VALUE ZEROS.
           05 WS-X           PIC 9(3) VALUE ZEROS.
       01 WS-SRC3.
           05 WS-ALPHA       PIC X(5) VALUE "HELLO".
           05 WS-BETA        PIC X(5) VALUE "WORLD".
           05 WS-GAMMA       PIC X(5) VALUE "COBOL".
       01 WS-DST3.
           05 WS-GAMMA       PIC X(5) VALUE SPACES.
           05 WS-ALPHA       PIC X(5) VALUE SPACES.
           05 WS-BETA        PIC X(5) VALUE SPACES.
           05 WS-DELTA       PIC X(5) VALUE "KEEP".
       PROCEDURE DIVISION.
       NC261A-CONTROL.
           PERFORM NC261A-TEST-1
           PERFORM NC261A-TEST-2
           PERFORM NC261A-TEST-3
           STOP RUN.
       NC261A-TEST-1.
      * MOVE CORR with alphanumeric and numeric fields reordered
      *   SRC has CITY STATE ZIP in order
      *   DST has ZIP CITY STATE in different order
      *   All three should match after MOVE CORRESPONDING
           MOVE CORRESPONDING WS-GROUP-SRC TO WS-GROUP-DST.
           IF WS-CITY OF WS-GROUP-DST = "DENVER"
               AND WS-STATE OF WS-GROUP-DST = "CO"
               AND WS-ZIP OF WS-GROUP-DST = 80202
               DISPLAY "NC261A-TEST-1 PASS"
           ELSE
               DISPLAY "NC261A-TEST-1 FAIL"
               DISPLAY "  CITY=" WS-CITY OF WS-GROUP-DST
               DISPLAY "  STATE=" WS-STATE OF WS-GROUP-DST
               DISPLAY "  ZIP=" WS-ZIP OF WS-GROUP-DST
           END-IF.
       NC261A-TEST-2.
      * MOVE CORR where DST has fewer fields than SRC
      *   SRC has X Y Z; DST has only Z X
      *   X should be 100, Z should be 300, Y is not in DST
           MOVE ZEROS TO WS-Z OF WS-DST2.
           MOVE ZEROS TO WS-X OF WS-DST2.
           MOVE CORRESPONDING WS-SRC2 TO WS-DST2.
           IF WS-X OF WS-DST2 = 100
               AND WS-Z OF WS-DST2 = 300
               DISPLAY "NC261A-TEST-2 PASS"
           ELSE
               DISPLAY "NC261A-TEST-2 FAIL"
               DISPLAY "  X=" WS-X OF WS-DST2
               DISPLAY "  Z=" WS-Z OF WS-DST2
           END-IF.
       NC261A-TEST-3.
      * MOVE CORR where DST has extra field DELTA
      *   SRC has ALPHA BETA GAMMA
      *   DST has GAMMA ALPHA BETA DELTA
      *   After: ALPHA=HELLO BETA=WORLD GAMMA=COBOL DELTA=KEEP
           MOVE SPACES TO WS-GAMMA OF WS-DST3.
           MOVE SPACES TO WS-ALPHA OF WS-DST3.
           MOVE SPACES TO WS-BETA OF WS-DST3.
           MOVE "KEEP" TO WS-DELTA OF WS-DST3.
           MOVE CORRESPONDING WS-SRC3 TO WS-DST3.
           IF WS-ALPHA OF WS-DST3 = "HELLO"
               AND WS-BETA OF WS-DST3 = "WORLD"
               AND WS-GAMMA OF WS-DST3 = "COBOL"
               AND WS-DELTA OF WS-DST3 = "KEEP"
               DISPLAY "NC261A-TEST-3 PASS"
           ELSE
               DISPLAY "NC261A-TEST-3 FAIL"
               DISPLAY "  ALPHA=" WS-ALPHA OF WS-DST3
               DISPLAY "  BETA=" WS-BETA OF WS-DST3
               DISPLAY "  GAMMA=" WS-GAMMA OF WS-DST3
               DISPLAY "  DELTA=" WS-DELTA OF WS-DST3
           END-IF.
