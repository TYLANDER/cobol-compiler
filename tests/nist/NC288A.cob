       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC288A.
      *
      * NIST CCVS-style test: Nested IF/ELSE with EVALUATE inside
      * Tests nested IF statements containing EVALUATE to verify
      * correct multi-level conditional dispatch.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CODE          PIC X(1) VALUE SPACE.
       01 WS-LEVEL         PIC 9(1) VALUE 0.
       01 WS-ACTIVE        PIC 9(1) VALUE 0.
       01 WS-RESULT        PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      * Test 1: Outer IF true, EVALUATE selects first WHEN
      *   ACTIVE=1, CODE="A", LEVEL=1 => "HIGH-A"
           MOVE 1 TO WS-ACTIVE.
           MOVE "A" TO WS-CODE.
           MOVE 1 TO WS-LEVEL.
           MOVE SPACES TO WS-RESULT.
           IF WS-ACTIVE = 1
               EVALUATE WS-CODE
                   WHEN "A"
                       MOVE "HIGH-A" TO WS-RESULT
                   WHEN "B"
                       MOVE "HIGH-B" TO WS-RESULT
                   WHEN OTHER
                       MOVE "HIGH-OTH" TO WS-RESULT
               END-EVALUATE
           ELSE
               MOVE "INACTIVE" TO WS-RESULT
           END-IF.
           IF WS-RESULT(1:6) = "HIGH-A"
               DISPLAY "NC288A-TEST-1 PASS"
           ELSE
               DISPLAY "NC288A-TEST-1 FAIL"
               DISPLAY "  Expected HIGH-A, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 2: Outer IF false, ELSE branch taken
      *   ACTIVE=0 => "INACTIVE"
           MOVE 0 TO WS-ACTIVE.
           MOVE "A" TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           IF WS-ACTIVE = 1
               EVALUATE WS-CODE
                   WHEN "A"
                       MOVE "HIGH-A" TO WS-RESULT
                   WHEN OTHER
                       MOVE "HIGH-OTH" TO WS-RESULT
               END-EVALUATE
           ELSE
               MOVE "INACTIVE" TO WS-RESULT
           END-IF.
           IF WS-RESULT(1:8) = "INACTIVE"
               DISPLAY "NC288A-TEST-2 PASS"
           ELSE
               DISPLAY "NC288A-TEST-2 FAIL"
               DISPLAY "  Expected INACTIVE, got >"
                   WS-RESULT "<"
           END-IF.
      * Test 3: Nested IF with EVALUATE inside ELSE
      *   ACTIVE=1, CODE="Z" => outer IF true, EVALUATE OTHER
           MOVE 1 TO WS-ACTIVE.
           MOVE "Z" TO WS-CODE.
           MOVE SPACES TO WS-RESULT.
           IF WS-ACTIVE = 1
               IF WS-CODE = "A"
                   MOVE "IS-A" TO WS-RESULT
               ELSE
                   EVALUATE WS-CODE
                       WHEN "B"
                           MOVE "IS-B" TO WS-RESULT
                       WHEN "C"
                           MOVE "IS-C" TO WS-RESULT
                       WHEN OTHER
                           MOVE "IS-OTHER" TO WS-RESULT
                   END-EVALUATE
               END-IF
           ELSE
               MOVE "OFF" TO WS-RESULT
           END-IF.
           IF WS-RESULT(1:8) = "IS-OTHER"
               DISPLAY "NC288A-TEST-3 PASS"
           ELSE
               DISPLAY "NC288A-TEST-3 FAIL"
               DISPLAY "  Expected IS-OTHER, got >"
                   WS-RESULT "<"
           END-IF.
           STOP RUN.
