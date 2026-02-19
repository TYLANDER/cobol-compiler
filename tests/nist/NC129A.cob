       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC129A.
      *
      * NIST CCVS-style test: Level-88 condition names
      * Tests 88-level VALUE clause with single value,
      * VALUE THRU range, and SET condition-name TO TRUE.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STATUS       PIC 9(2) VALUE ZEROS.
           88 STATUS-ACTIVE    VALUE 1.
           88 STATUS-INACTIVE  VALUE 0.
           88 STATUS-PENDING   VALUE 5.
       01 WS-GRADE         PIC 9(3) VALUE ZEROS.
           88 GRADE-PASS       VALUE 60 THRU 100.
           88 GRADE-FAIL       VALUE 0 THRU 59.
       01 WS-FLAG          PIC 9    VALUE 0.
           88 FLAG-ON          VALUE 1.
           88 FLAG-OFF         VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: 88-level with single value check
           MOVE 1 TO WS-STATUS.
           IF STATUS-ACTIVE
               DISPLAY "NC129A-TEST-1 PASS"
           ELSE
               DISPLAY "NC129A-TEST-1 FAIL"
               DISPLAY "  STATUS=" WS-STATUS
                   " expected ACTIVE"
           END-IF.
      * Test 2: 88-level with THRU range
           MOVE 75 TO WS-GRADE.
           IF GRADE-PASS
               DISPLAY "NC129A-TEST-2 PASS"
           ELSE
               DISPLAY "NC129A-TEST-2 FAIL"
               DISPLAY "  GRADE=" WS-GRADE
                   " expected PASS range"
           END-IF.
      * Test 3: SET condition-name TO TRUE
           MOVE 0 TO WS-FLAG.
           SET FLAG-ON TO TRUE.
           IF FLAG-ON AND WS-FLAG = 1
               DISPLAY "NC129A-TEST-3 PASS"
           ELSE
               DISPLAY "NC129A-TEST-3 FAIL"
               DISPLAY "  FLAG=" WS-FLAG
                   " expected 1"
           END-IF.
           STOP RUN.
