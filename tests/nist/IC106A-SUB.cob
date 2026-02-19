       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC106A-SUB.
      *
      * Subprogram for IC106A: adds 10 to a numeric parameter.
      * Called multiple times to verify cumulative effect.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-COUNTER PIC 9(4).
       PROCEDURE DIVISION USING LS-COUNTER.
           ADD 10 TO LS-COUNTER.
