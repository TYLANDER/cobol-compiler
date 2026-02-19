       IDENTIFICATION DIVISION.
       PROGRAM-ID. GOBACKSUB.
      * Subprogram with no USING or LINKAGE. GOBACK should return.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-LOCAL        PIC X(10) VALUE "LOCAL".
       PROCEDURE DIVISION.
           DISPLAY "SUB-START".
           GOBACK.
