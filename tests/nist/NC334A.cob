       IDENTIFICATION DIVISION.
       PROGRAM-ID. NC334A.
      *
      * NIST CCVS-style test: Qualification with OF/IN.
      * Tests accessing identically named fields in different groups
      * using the OF/IN qualification syntax.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-EMPLOYEE.
          05 WS-PERSONAL.
             10 WS-NAME    PIC X(10) VALUE "JOHN      ".
             10 WS-CODE    PIC 9(3)  VALUE 100.
          05 WS-WORK.
             10 WS-NAME    PIC X(10) VALUE "ENGINEER  ".
             10 WS-CODE    PIC 9(3)  VALUE 200.
       01 WS-CUSTOMER.
          05 WS-DETAILS.
             10 WS-NAME    PIC X(10) VALUE "ACME CORP ".
             10 WS-CODE    PIC 9(3)  VALUE 300.
       01 WS-RESULT        PIC X(10) VALUE SPACES.
       01 WS-NUM-RESULT    PIC 9(3)  VALUE 0.
       PROCEDURE DIVISION.
      * Test 1: Qualify WS-NAME OF WS-PERSONAL OF WS-EMPLOYEE
      *   Should access "JOHN      "
           MOVE WS-NAME OF WS-PERSONAL OF WS-EMPLOYEE
               TO WS-RESULT.
           IF WS-RESULT = "JOHN      "
               DISPLAY "NC334A-TEST-1 PASS"
           ELSE
               DISPLAY "NC334A-TEST-1 FAIL"
               DISPLAY "  Expected 'JOHN      ', got '"
                   WS-RESULT "'"
           END-IF.
      * Test 2: Qualify WS-NAME IN WS-WORK IN WS-EMPLOYEE
      *   Should access "ENGINEER  " (using IN instead of OF)
           MOVE WS-NAME IN WS-WORK IN WS-EMPLOYEE
               TO WS-RESULT.
           IF WS-RESULT = "ENGINEER  "
               DISPLAY "NC334A-TEST-2 PASS"
           ELSE
               DISPLAY "NC334A-TEST-2 FAIL"
               DISPLAY "  Expected 'ENGINEER  ', got '"
                   WS-RESULT "'"
           END-IF.
      * Test 3: Qualify WS-CODE OF WS-DETAILS OF WS-CUSTOMER
      *   Should access 300
           MOVE WS-CODE OF WS-DETAILS OF WS-CUSTOMER
               TO WS-NUM-RESULT.
           IF WS-NUM-RESULT = 300
               DISPLAY "NC334A-TEST-3 PASS"
           ELSE
               DISPLAY "NC334A-TEST-3 FAIL"
               DISPLAY "  Expected 300, got " WS-NUM-RESULT
           END-IF.
           STOP RUN.
