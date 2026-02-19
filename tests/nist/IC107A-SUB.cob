       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC107A-SUB.
      *
      * Subprogram for IC107A: multiplication with multiple params.
      * Receives two input numbers and one output field.
      * Multiplies the two inputs and stores the result in output.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-X PIC 9(4).
       01 LS-Y PIC 9(4).
       01 LS-RESULT PIC 9(4).
       PROCEDURE DIVISION USING LS-X LS-Y LS-RESULT.
           MULTIPLY LS-X BY LS-Y GIVING LS-RESULT.
