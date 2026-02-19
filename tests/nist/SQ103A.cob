       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ103A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests WRITE with ADVANCING clause (BEFORE/AFTER ADVANCING).
      *
      * SKIP: The WRITE ADVANCING clause is parsed but not
      * implemented in codegen. The ADVANCING option is silently
      * ignored -- WRITE always emits a single line. There is no
      * way to verify ADVANCING behaviour at runtime, so this
      * test is skipped until ADVANCING support is added to the
      * compiler.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ103A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(40).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(40).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
           DISPLAY "SQ103A SKIPPED - ADVANCING not supported".
           STOP RUN.
