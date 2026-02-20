      *IC219A-SUB - SUB-PROGRAM FOR MULTIPLE CALL TEST
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC219A-SUB.
       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-INPUT-VAL          PIC 9(4).
       01  LS-MULTIPLIER         PIC 9(4).
       01  LS-OUTPUT-VAL         PIC 9(8).
       PROCEDURE DIVISION USING LS-INPUT-VAL
                                 LS-MULTIPLIER
                                 LS-OUTPUT-VAL.
       IC219A-SUB-MAIN.
           MULTIPLY LS-INPUT-VAL BY LS-MULTIPLIER
               GIVING LS-OUTPUT-VAL
           END-MULTIPLY
           GOBACK.
