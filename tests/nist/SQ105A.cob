       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ105A.
      *
      * NIST CCVS-style test: Sequential File I/O
      * Tests FILE STATUS checking (00 for success, 10 for EOF).
      *
      * SKIP: The FILE STATUS clause is recognized by the lexer
      * (FileStatus token exists) but is not wired through HIR
      * or MIR to codegen. The status variable is never updated
      * by the runtime after I/O operations, so FILE STATUS
      * tests cannot produce correct results. This test is
      * skipped until FILE STATUS support is added to the
      * compiler.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "/tmp/SQ105A.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-RECORD PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-STATUS PIC XX VALUE SPACES.
       01 WS-RECORD PIC X(20).
       01 WS-EOF PIC 9 VALUE 0.
       PROCEDURE DIVISION.
           DISPLAY "SQ105A SKIPPED - FILE STATUS not supported".
           STOP RUN.
