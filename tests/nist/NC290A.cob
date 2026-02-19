       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC290A.
      *
      * NIST CCVS-style test: INSPECT REPLACING ALL/LEADING/FIRST
      * with BEFORE/AFTER phrases. Tests INSPECT REPLACING
      * combined with positional qualifiers.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA          PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: INSPECT REPLACING ALL with BEFORE INITIAL
      *   "AABXAAYAA" replace ALL "A" BY "Z" BEFORE "X"
      *   Region before "X" is "AAB", after is "XAAYAA"
      *   Only "AA" before X replaced => "ZZBXAAYAA"
           MOVE "AABXAAYAA" TO WS-DATA.
           INSPECT WS-DATA REPLACING ALL "A" BY "Z"
               BEFORE INITIAL "X".
           IF WS-DATA(1:9) = "ZZBXAAYAA"
               DISPLAY "NC290A-TEST-1 PASS"
           ELSE
               DISPLAY "NC290A-TEST-1 FAIL"
               DISPLAY "  Expected ZZBXAAYAA, got >"
                   WS-DATA "<"
           END-IF.
      * Test 2: INSPECT REPLACING LEADING with AFTER INITIAL
      *   "XXAAXBBBB" replace LEADING "B" by "Z" AFTER "X"
      *   After first "X" is "XAAXBBBB"
      *   Leading "B" in that region: none (starts with "X")
      *   => no change
           MOVE "XXAAXBBBB" TO WS-DATA.
           INSPECT WS-DATA REPLACING LEADING "B" BY "Z"
               AFTER INITIAL "X".
           IF WS-DATA(1:9) = "XXAAXBBBB"
               DISPLAY "NC290A-TEST-2 PASS"
           ELSE
               DISPLAY "NC290A-TEST-2 FAIL"
               DISPLAY "  Expected XXAAXBBBB, got >"
                   WS-DATA "<"
           END-IF.
      * Test 3: INSPECT REPLACING FIRST with BEFORE INITIAL
      *   "ABCABCABC" replace FIRST "A" BY "X" BEFORE "C"
      *   Region before first "C" is "AB", first "A" is pos 1
      *   => "XBCABCABC"
           MOVE "ABCABCABC" TO WS-DATA.
           INSPECT WS-DATA REPLACING FIRST "A" BY "X"
               BEFORE INITIAL "C".
           IF WS-DATA(1:9) = "XBCABCABC"
               DISPLAY "NC290A-TEST-3 PASS"
           ELSE
               DISPLAY "NC290A-TEST-3 FAIL"
               DISPLAY "  Expected XBCABCABC, got >"
                   WS-DATA "<"
           END-IF.
           STOP RUN.
