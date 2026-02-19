       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC297A.
      *
      * NIST CCVS-style test: INSPECT CONVERTING
      * Tests INSPECT CONVERTING to transliterate characters
      * in a string field.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA1        PIC X(10) VALUE SPACES.
       01 WS-DATA2        PIC X(10) VALUE SPACES.
       01 WS-DATA3        PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Convert lowercase to uppercase
      *   "abcXYZ1234" => "ABCXYZ1234"
           MOVE "abcXYZ1234" TO WS-DATA1.
           INSPECT WS-DATA1
               CONVERTING "abcdefghij" TO "ABCDEFGHIJ".
           IF WS-DATA1 = "ABCXYZ1234"
               DISPLAY "NC297A-TEST-1 PASS"
           ELSE
               DISPLAY "NC297A-TEST-1 FAIL"
               DISPLAY "  Expected [ABCXYZ1234] got ["
                   WS-DATA1 "]"
           END-IF.
      * Test 2: Convert digits to symbols
      *   "A1B2C3DDDD" => "A!B@C#DDDD"
           MOVE "A1B2C3DDDD" TO WS-DATA2.
           INSPECT WS-DATA2
               CONVERTING "123" TO "!@#".
           IF WS-DATA2 = "A!B@C#DDDD"
               DISPLAY "NC297A-TEST-2 PASS"
           ELSE
               DISPLAY "NC297A-TEST-2 FAIL"
               DISPLAY "  Expected [A!B@C#DDDD] got ["
                   WS-DATA2 "]"
           END-IF.
      * Test 3: Convert with no matching chars (no change)
      *   "HELLO     " converting "xyz" to "XYZ" => no change
           MOVE "HELLO" TO WS-DATA3.
           INSPECT WS-DATA3
               CONVERTING "xyz" TO "XYZ".
           IF WS-DATA3 = "HELLO     "
               DISPLAY "NC297A-TEST-3 PASS"
           ELSE
               DISPLAY "NC297A-TEST-3 FAIL"
               DISPLAY "  Expected [HELLO     ] got ["
                   WS-DATA3 "]"
           END-IF.
           STOP RUN.
