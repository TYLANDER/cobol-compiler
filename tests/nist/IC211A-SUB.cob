       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC211A-SUB.
      *
      * Subprogram for IC211A: BY REFERENCE parameter modification.
      * Modifies all three parameters to verify BY REFERENCE
      * passing allows the caller to see changes.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CALL-COUNT PIC 9(4) VALUE 0.
       LINKAGE SECTION.
       01 LS-NUM    PIC 9(4).
       01 LS-ALPHA  PIC X(10).
       01 LS-FLAG   PIC 9(4).
       PROCEDURE DIVISION USING LS-NUM LS-ALPHA LS-FLAG.
           ADD 1 TO WS-CALL-COUNT.
           MOVE 999 TO LS-NUM.
           MOVE "REFCHANGE " TO LS-ALPHA.
           MOVE WS-CALL-COUNT TO LS-FLAG.
           GOBACK.
