       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC214A-SUB.
      *
      * Subprogram for IC214A: first level of chained call.
      * Doubles the value, records trace, then calls IC214A-SUB2
      * which triples the value and appends to trace.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-VALUE PIC 9(4).
       01 LS-TRACE PIC X(20).
       PROCEDURE DIVISION USING LS-VALUE LS-TRACE.
           MULTIPLY LS-VALUE BY 2 GIVING LS-VALUE.
           MOVE "LEVEL1-" TO LS-TRACE.
           CALL "IC214A-SUB2" USING LS-VALUE LS-TRACE.
           GOBACK.
