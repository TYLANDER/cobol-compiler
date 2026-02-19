       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMOKE-INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME     PIC X(10) VALUE "Hello     ".
       01 WS-RESULT   PIC X(10) VALUE SPACES.
       01 WS-LEN      PIC 9(3)  VALUE ZEROS.
       01 WS-NUM-A    PIC 9(3)  VALUE 042.
       01 WS-NUM-B    PIC 9(3)  VALUE 099.
       01 WS-NUM-C    PIC 9(3)  VALUE 007.
       PROCEDURE DIVISION.
           DISPLAY "--- INTRINSIC FUNCTIONS ---".
      *    FUNCTION LENGTH
           COMPUTE WS-LEN =
               FUNCTION LENGTH(WS-NAME).
           DISPLAY WS-LEN.
      *    FUNCTION UPPER-CASE
           MOVE FUNCTION UPPER-CASE(WS-NAME)
               TO WS-RESULT.
           DISPLAY WS-RESULT.
      *    FUNCTION LOWER-CASE
           MOVE FUNCTION LOWER-CASE(WS-NAME)
               TO WS-RESULT.
           DISPLAY WS-RESULT.
      *    FUNCTION REVERSE
           MOVE FUNCTION REVERSE(WS-NAME)
               TO WS-RESULT.
           DISPLAY WS-RESULT.
      *    FUNCTION TRIM
           MOVE FUNCTION TRIM(WS-NAME)
               TO WS-RESULT.
           DISPLAY WS-RESULT.
      *    FUNCTION MAX
           COMPUTE WS-LEN =
               FUNCTION MAX(WS-NUM-A, WS-NUM-B).
           DISPLAY WS-LEN.
      *    FUNCTION MIN
           COMPUTE WS-LEN =
               FUNCTION MIN(WS-NUM-A, WS-NUM-C).
           DISPLAY WS-LEN.
           STOP RUN.
