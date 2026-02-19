       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC221A.
      *
      * NIST CCVS-style test: MOVE SPACES/ZEROS to group and
      * elementary items. Tests that MOVE SPACES fills alphanumeric
      * fields and MOVE ZEROS fills numeric fields correctly,
      * including group-level MOVEs.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP1.
           05 WS-G1-ALPHA     PIC X(5)  VALUE "HELLO".
           05 WS-G1-ALPHA2    PIC X(3)  VALUE "XYZ".
       01 WS-ELEM-ALPHA       PIC X(8)  VALUE "ABCDEFGH".
       01 WS-ELEM-NUM         PIC 9(4)  VALUE 1234.
       01 WS-GROUP2.
           05 WS-G2-NUM1      PIC 9(3)  VALUE 111.
           05 WS-G2-NUM2      PIC 9(3)  VALUE 222.
       PROCEDURE DIVISION.
      * Test 1: MOVE SPACES to a group item
      *   Both subordinate alphanumeric fields should become spaces
           MOVE SPACES TO WS-GROUP1.
           IF WS-G1-ALPHA = SPACES
               AND WS-G1-ALPHA2 = SPACES
               DISPLAY "NC221A-TEST-1 PASS"
           ELSE
               DISPLAY "NC221A-TEST-1 FAIL"
               DISPLAY "  G1-A=>" WS-G1-ALPHA "<"
               DISPLAY "  G1-A2=>" WS-G1-ALPHA2 "<"
           END-IF.
      * Test 2: MOVE SPACES to elementary alphanumeric item
           MOVE SPACES TO WS-ELEM-ALPHA.
           IF WS-ELEM-ALPHA = SPACES
               DISPLAY "NC221A-TEST-2 PASS"
           ELSE
               DISPLAY "NC221A-TEST-2 FAIL"
               DISPLAY "  ELEM=>" WS-ELEM-ALPHA "<"
           END-IF.
      * Test 3: MOVE ZEROS to elementary numeric item
           MOVE ZEROS TO WS-ELEM-NUM.
           IF WS-ELEM-NUM = 0
               DISPLAY "NC221A-TEST-3 PASS"
           ELSE
               DISPLAY "NC221A-TEST-3 FAIL"
               DISPLAY "  NUM=" WS-ELEM-NUM
           END-IF.
           STOP RUN.
