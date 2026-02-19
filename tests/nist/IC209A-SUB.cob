       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC209A-SUB.
      *
      * Subprogram for IC209A: multi-call dispatch test.
      * Performs ADD, SUB, or MUL based on the action code,
      * applying the input value to the result parameter.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-ACTION PIC X(4).
       01 LS-INPUT  PIC 9(4).
       01 LS-RESULT PIC 9(4).
       PROCEDURE DIVISION USING LS-ACTION LS-INPUT LS-RESULT.
           IF LS-ACTION = "ADD "
               ADD LS-INPUT TO LS-RESULT
           END-IF.
           IF LS-ACTION = "SUB "
               SUBTRACT LS-INPUT FROM LS-RESULT
           END-IF.
           IF LS-ACTION = "MUL "
               MULTIPLY LS-INPUT BY LS-RESULT
                   GIVING LS-RESULT
           END-IF.
