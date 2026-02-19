       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC216A-SUB.
      *
      * Subprogram for IC216A: group item parameter test.
      * Receives a group item and populates it as a whole,
      * to verify the caller can see changes to the group
      * and its individual sub-fields.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-RECORD  PIC X(20).
       01 LS-FLAG    PIC 9(4).
       PROCEDURE DIVISION USING LS-RECORD LS-FLAG.
           MOVE "GROUPTEST GRP1005000" TO LS-RECORD.
           MOVE 1 TO LS-FLAG.
           GOBACK.
