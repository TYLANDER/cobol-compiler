       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC368A.
      *
      * NIST CCVS-style test: INSPECT REPLACING with
      * BEFORE/AFTER INITIAL phrases.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA-1       PIC X(20) VALUE SPACES.
       01 WS-DATA-2       PIC X(20) VALUE SPACES.
       01 WS-DATA-3       PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: INSPECT REPLACING ALL "X" BY "Y" BEFORE "Z"
      * "XXZXX" -> replace X by Y only before first Z
      * Expected: "YYZXX"
           MOVE "XXZXX" TO WS-DATA-1.
           INSPECT WS-DATA-1 REPLACING
               ALL "X" BY "Y" BEFORE INITIAL "Z".
           IF WS-DATA-1 = "YYZXX"
               DISPLAY "NC368A-TEST-1 PASS"
           ELSE
               DISPLAY "NC368A-TEST-1 FAIL"
               DISPLAY "  Expected YYZXX, got [" WS-DATA-1 "]"
           END-IF.
      * Test 2: INSPECT REPLACING ALL "X" BY "Y" AFTER "Z"
      * "XXZXX" -> replace X by Y only after first Z
      * Expected: "XXZYY"
           MOVE "XXZXX" TO WS-DATA-2.
           INSPECT WS-DATA-2 REPLACING
               ALL "X" BY "Y" AFTER INITIAL "Z".
           IF WS-DATA-2 = "XXZYY"
               DISPLAY "NC368A-TEST-2 PASS"
           ELSE
               DISPLAY "NC368A-TEST-2 FAIL"
               DISPLAY "  Expected XXZYY, got [" WS-DATA-2 "]"
           END-IF.
      * Test 3: INSPECT REPLACING FIRST "A" BY "B" BEFORE "C"
      * "AADACA" -> replace first A by B before first C
      * Expected: "BADACA"
           MOVE "AADACA" TO WS-DATA-3.
           INSPECT WS-DATA-3 REPLACING
               FIRST "A" BY "B" BEFORE INITIAL "C".
           IF WS-DATA-3 = "BADACA"
               DISPLAY "NC368A-TEST-3 PASS"
           ELSE
               DISPLAY "NC368A-TEST-3 FAIL"
               DISPLAY "  Expected BADACA, got [" WS-DATA-3 "]"
           END-IF.
           STOP RUN.
