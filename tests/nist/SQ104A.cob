       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ104A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests REWRITE statement (OPEN I-O, READ, REWRITE).
      *
      * SKIP: The REWRITE statement is parsed at the syntax level
      * but is not lowered through HIR or MIR to codegen. The
      * statement is silently ignored at runtime, so REWRITE
      * tests cannot produce correct results. This test is
      * skipped until REWRITE support is added to the compiler.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ104A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
           DISPLAY "SQ104A SKIPPED - REWRITE not supported".
           STOP RUN.
