       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC332A.
      *
      * NIST CCVS-style test: INSPECT REPLACING LEADING/TRAILING
      * with BEFORE/AFTER phrases.
      * Tests INSPECT REPLACING LEADING and TRAILING combined with
      * BEFORE INITIAL and AFTER INITIAL delimiters.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA1        PIC X(15) VALUE SPACES.
       01 WS-DATA2        PIC X(15) VALUE SPACES.
       01 WS-DATA3        PIC X(15) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: INSPECT REPLACING LEADING "0" BY " " BEFORE "."
      *   "000123.00456" => leading 0's before "." replaced:
      *   "   123.00456"
           MOVE "000123.00456   " TO WS-DATA1.
           INSPECT WS-DATA1 REPLACING
               LEADING "0" BY " " BEFORE INITIAL ".".
           IF WS-DATA1 = "   123.00456   "
               DISPLAY "NC332A-TEST-1 PASS"
           ELSE
               DISPLAY "NC332A-TEST-1 FAIL"
               DISPLAY "  Expected '   123.00456   '"
               DISPLAY "  Got      '" WS-DATA1 "'"
           END-IF.
      * Test 2: INSPECT REPLACING ALL "0" BY " " AFTER "."
      *   "123.40500      " => 0's after "." replaced by " ":
      *   "123.4 5        "
           MOVE "123.40500      " TO WS-DATA2.
           INSPECT WS-DATA2 REPLACING
               ALL "0" BY " " AFTER INITIAL ".".
           IF WS-DATA2 = "123.4 5        "
               DISPLAY "NC332A-TEST-2 PASS"
           ELSE
               DISPLAY "NC332A-TEST-2 FAIL"
               DISPLAY "  Expected '123.4 5        '"
               DISPLAY "  Got      '" WS-DATA2 "'"
           END-IF.
      * Test 3: INSPECT REPLACING LEADING "X" BY "Y" AFTER "A"
      *   "XXXAXXXBXX" => after first A: "XXXAYYYBXX"
      *   Leading X's after A are the three X's before B.
           MOVE "XXXAXXXBXX     " TO WS-DATA3.
           INSPECT WS-DATA3 REPLACING
               LEADING "X" BY "Y" AFTER INITIAL "A".
           IF WS-DATA3 = "XXXAYYYBXX     "
               DISPLAY "NC332A-TEST-3 PASS"
           ELSE
               DISPLAY "NC332A-TEST-3 FAIL"
               DISPLAY "  Expected 'XXXAYYYBXX     '"
               DISPLAY "  Got      '" WS-DATA3 "'"
           END-IF.
           STOP RUN.
