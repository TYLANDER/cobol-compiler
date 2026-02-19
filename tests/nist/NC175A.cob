       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC175A.
      *
      * NIST CCVS-style test: INSPECT CONVERTING
      * Tests the INSPECT statement with CONVERTING clause
      * to perform character transliteration in-place.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA1         PIC X(10) VALUE SPACES.
       01 WS-DATA2         PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Convert "ABC" to "XYZ"
      *   "AABBCC" => "XXYYZZ"
           MOVE "AABBCCDDEE" TO WS-DATA1.
           INSPECT WS-DATA1
               CONVERTING "ABC" TO "XYZ".
           IF WS-DATA1 = "XXYYZZDDEE"
               DISPLAY "NC175A-TEST-1 PASS"
           ELSE
               DISPLAY "NC175A-TEST-1 FAIL"
               DISPLAY "  Expected [XXYYZZDDEE] got ["
                   WS-DATA1 "]"
           END-IF.
      * Test 2: Convert lowercase to uppercase (partial)
      *   Convert "abc" to "ABC"
           MOVE "aXbYcZ1234" TO WS-DATA2.
           INSPECT WS-DATA2
               CONVERTING "abc" TO "ABC".
           IF WS-DATA2 = "AXBYCZ1234"
               DISPLAY "NC175A-TEST-2 PASS"
           ELSE
               DISPLAY "NC175A-TEST-2 FAIL"
               DISPLAY "  Expected [AXBYCZ1234] got ["
                   WS-DATA2 "]"
           END-IF.
           STOP RUN.
