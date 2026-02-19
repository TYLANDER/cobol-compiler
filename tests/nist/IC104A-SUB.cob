       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC104A-SUB.
      *
      * Subprogram for IC104A: arithmetic in subprogram.
      * Receives two numbers, computes their sum.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-A PIC 9(4).
       01 LS-B PIC 9(4).
       01 LS-SUM PIC 9(4).
       PROCEDURE DIVISION USING LS-A LS-B LS-SUM.
           ADD LS-A LS-B GIVING LS-SUM.
