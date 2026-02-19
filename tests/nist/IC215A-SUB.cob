       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC215A-SUB.
      *
      * Subprogram for IC215A: numeric parameter computation.
      * Receives price and quantity, computes total (price * qty)
      * and tax (total / 10), returning both via BY REFERENCE.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TEMP  PIC 9(6) VALUE 0.
       LINKAGE SECTION.
       01 LS-PRICE  PIC 9(4).
       01 LS-QTY    PIC 9(4).
       01 LS-TOTAL  PIC 9(6).
       01 LS-TAX    PIC 9(6).
       PROCEDURE DIVISION USING LS-PRICE LS-QTY LS-TOTAL LS-TAX.
           MULTIPLY LS-PRICE BY LS-QTY GIVING WS-TEMP.
           MOVE WS-TEMP TO LS-TOTAL.
           DIVIDE WS-TEMP BY 10 GIVING WS-TEMP.
           MOVE WS-TEMP TO LS-TAX.
           GOBACK.
