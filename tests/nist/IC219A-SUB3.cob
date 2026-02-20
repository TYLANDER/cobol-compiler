       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC219A-SUB3.
      *
      * Subprogram 3 for IC219A: flag toggle.
      * Sets the flag parameter to 1.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-FLAG PIC 9.
       PROCEDURE DIVISION USING LS-FLAG.
           MOVE 1 TO LS-FLAG.
           GOBACK.
