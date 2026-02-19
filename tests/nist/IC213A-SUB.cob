       IDENTIFICATION DIVISION.
       PROGRAM-ID. IC213A-SUB.
      *
      * Subprogram for IC213A: mixed BY REFERENCE / BY CONTENT.
      * Reads BY CONTENT parameters to verify received values,
      * modifies only BY REFERENCE parameters to signal success.
      * NOTE: Does not modify BY CONTENT params to avoid issues
      * where BY CONTENT is treated as BY REFERENCE.
      *
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-REF-ALPHA PIC X(10).
       01 LS-CNT-ALPHA PIC X(10).
       01 LS-REF-NUM   PIC 9(4).
       01 LS-CNT-NUM   PIC 9(4).
       01 LS-FLAG      PIC 9(4).
       PROCEDURE DIVISION USING LS-REF-ALPHA LS-CNT-ALPHA
                                LS-REF-NUM LS-CNT-NUM LS-FLAG.
      * Modify BY REFERENCE parameters
           MOVE "MIXED-REF " TO LS-REF-ALPHA.
           MOVE 8888 TO LS-REF-NUM.
      * Verify BY CONTENT values received correctly, set flag
           IF LS-CNT-ALPHA = "PROTECT   "
             AND LS-CNT-NUM = 5555
               MOVE 1 TO LS-FLAG
           ELSE
               MOVE 0 TO LS-FLAG
           END-IF.
           GOBACK.
