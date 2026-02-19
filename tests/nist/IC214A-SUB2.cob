       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC214A-SUB2.
      *
      * Subprogram for IC214A: second level of chained call.
      * Triples the value and appends to trace string to
      * confirm this subprogram was reached in the chain.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-VALUE PIC 9(4).
       01 LS-TRACE PIC X(20).
       PROCEDURE DIVISION USING LS-VALUE LS-TRACE.
           MULTIPLY LS-VALUE BY 3 GIVING LS-VALUE.
           MOVE "LEVEL1-LEVEL2       " TO LS-TRACE.
           GOBACK.
