       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC205A-SUB.
      *
      * Subprogram for IC205A: Multi-operation dispatch.
      * Receives an operation code, an input value, and an
      * output value. Performs different arithmetic based on
      * the operation code.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-OP     PIC X(4).
       01 LS-INPUT  PIC 9(4).
       01 LS-OUTPUT PIC 9(4).
       PROCEDURE DIVISION USING LS-OP LS-INPUT LS-OUTPUT.
           IF LS-OP = "ADD "
               ADD LS-INPUT TO LS-OUTPUT
           END-IF.
           IF LS-OP = "SUB "
               SUBTRACT LS-INPUT FROM LS-OUTPUT
           END-IF.
           IF LS-OP = "MUL "
               MULTIPLY LS-INPUT BY LS-OUTPUT
                   GIVING LS-OUTPUT
           END-IF.
