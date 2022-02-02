       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATCODER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYSIN ASSIGN TO KEYBOARD ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
           FD SYSIN.
               03 INP PIC X(2000000).
       WORKING-STORAGE SECTION.
           01 WK.
               03 INP-WK PIC X(2000000).
               03 I PIC 9(6).
               03 S PIC 9(10).
               03 T PIC 9(10).
               03 PT PIC 9(10) VALUE 1.
               03 OK PIC 9(10) VALUE 1000000000.
               03 NG PIC 9(10) VALUE 0.
               03 MID PIC 9(9).
               03 N PIC 9(6).
               03 K PIC 9(10).
               03 ANS-Z PIC Z(9)9.
           01 AL.
               03 AI OCCURS 1 TO 200000 TIMES DEPENDING ON N.
                   05 A PIC 9(10).
       PROCEDURE DIVISION.
           OPEN INPUT SYSIN.
           ACCEPT INP-WK.
           UNSTRING INP-WK DELIMITED BY SPACE INTO N K.
           READ SYSIN.
           CLOSE SYSIN.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               UNSTRING INP DELIMITED BY SPACE INTO A(I) WITH POINTER PT
           END-PERFORM.
           PERFORM UNTIL FUNCTION ABS(OK - NG) <= 1
               COMPUTE MID = (OK + NG) / 2
               MOVE ZERO TO S
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
                   COMPUTE S = S + (A(I) - 1) / MID
               END-PERFORM
               IF S <= K THEN
                   MOVE MID TO OK
               ELSE
                   MOVE MID TO NG
               END-IF
           END-PERFORM
           MOVE OK TO ANS-Z.
           DISPLAY FUNCTION TRIM(ANS-Z).
           STOP RUN.
       END PROGRAM ATCODER.
