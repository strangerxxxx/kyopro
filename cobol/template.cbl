       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATCODER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYSIN ASSIGN TO KEYBOARD ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
           FD SYSIN.
               01 INDATA PIC X(2000000).
       WORKING-STORAGE SECTION.
           01 WK.
               03 INP PIC X(2000000).
               03 I PIC 9(18).
               03 J PIC 9(18).
               03 N PIC 9(18).
               03 ANS PIC 9(17).
               03 ANS-Z PIC Z(14)9.
       PROCEDURE DIVISION.
           ACCEPT N.
           OPEN INPUT SYSIN.
           READ SYSIN INTO INP.
           CLOSE SYSIN.
           
           MOVE ANS TO ANS-Z.
           DISPLAY FUNCTION TRIM(ANS-Z).
           STOP RUN.
       END PROGRAM ATCODER.

      *    SPLIT LIST
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYSIN ASSIGN TO KEYBOARD ORGANIZATION LINE SEQUENTIAL.

       FILE SECTION.
           FD SYSIN.
               01 INDATA PIC X(2000000).

               03 I PIC 9(18).
               03 PT PIC 9(18).

           OPEN INPUT SYSIN.
           READ SYSIN INTO INP.
           CLOSE SYSIN.

      *    SPLIT
           UNSTRING INP DELIMITED BY SPACE INTO N M.

      *    SPLIT TO LIST
               03 I PIC 9(18).
               03 N PIC 9(18).
               03 PT PIC 9(18) VALUE 1.
           01 AL.
               03 A OCCURS 10000 TIMES PIC 9(5).

           MOVE 1 TO PT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               UNSTRING INP DELIMITED BY SPACE INTO A(I) WITH POINTER PT
           END-PERFORM.

      *    SORT
           01 AL.
               03 AI OCCURS 1 TO 100000 TIMES DEPENDING ON N.
                   05 A PIC 9(5).
           SORT AI ON DESCENDING KEY A.
           SORT AI ON ASCENDING KEY A.

      *    LEN
           MOVE FUNCTION STORED-CHAR-LENGTH(S) TO N.

      *    SPLIT TO LIST 2D
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               ACCEPT INP
               MOVE 1 TO PT
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
                   UNSTRING INP DELIMITED BY SPACE INTO A(I J) WITH POINTER PT
               END-PERFORM
           END-PERFORM.
           
      *    OUTPUT WITHOUT ADVANCING
           DISPLAY ANS WITH NO ADVANCING.
