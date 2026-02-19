       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC210A-SUB.
      *
      * Subprogram for IC210A: literal name with large parameter.
      * Sets a 30-byte alphanumeric field and a flag to confirm
      * the subprogram was called and data was transferred.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-BIG    PIC X(30).
       01 LS-FLAG   PIC 9(4).
       PROCEDURE DIVISION USING LS-BIG LS-FLAG.
           MOVE "LARGE-PARAMETER-VALUE-RECEIVED" TO LS-BIG.
           MOVE 1 TO LS-FLAG.
