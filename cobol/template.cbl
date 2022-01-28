       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATCODER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYSIN ASSIGN TO KEYBOARD ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
           FD SYSIN.
               01 INP PIC X(2000000).
       WORKING-STORAGE SECTION.
           01 WK.
               03 I PIC 9(18).
               03 J PIC 9(18).
               03 K PIC 9(18).
               03 N PIC 9(18).
               03 M PIC 9(18).
               03 ANS PIC 9(18).
               03 ANS-Z PIC Z(17)9.
               03 PT PIC 9(18) VALUE 1.
       PROCEDURE DIVISION.
           ACCEPT N.
           OPEN INPUT SYSIN.
           READ SYSIN.
           CLOSE SYSIN.
           UNSTRING INP DELIMITED BY SPACE INTO N M.
           MOVE ANS TO ANS-Z.
           DISPLAY FUNCTION TRIM(ANS-Z).
           STOP RUN.
       END PROGRAM ATCODER.

      *    SPLIT
           UNSTRING INP DELIMITED BY SPACE INTO N M.

      *    SPLIT TO LIST
               03 I PIC 9(18).
               03 N PIC 9(18).
               03 PT PIC 9(18) VALUE 1.
           01 AL.
               03 A OCCURS 10000 TIMES PIC 9(5).
       PROCEDURE DIVISION.
           OPEN INPUT SYSIN.
           READ SYSIN.
           MOVE 1 TO PT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               UNSTRING INP DELIMITED BY SPACE INTO A(I) WITH POINTER PT
           END-PERFORM.
           CLOSE SYSIN.

      *    SORT
           01 AL.
               03 AI OCCURS 1 TO 200000 TIMES DEPENDING ON N.
                   05 A PIC 9(10).
           SORT AI ON ASCENDING KEY A.
           SORT AI ON DESCENDING KEY A.

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

        *>    EVALUATE
           EVALUATE 式
           WHEN 値 文
           WHEN 値 文
           OTHER 文
           END-EVALUATE.

    *>      SECTION
       HOGE SECTION.
       EXIT.

    *>    ITER SECTION.
    *>        IF I = 10 THEN PERFORM SUB
    *>        ELSE
    *>        PERFORM VARYING J FROM 0 BY 1 UNTIL J > 1
    *>            MOVE J TO A(I)
    *>            ADD 1 TO I
    *>            PERFORM ITER
    *>            SUBTRACT 1 FROM I
    *>        END-PERFORM
    *>        END-IF.
    *>    EXIT.

    *>    IDENTIFICATION DIVISION.
    *>    PROGRAM-ID. SUB.
    *>    ENVIRONMENT DIVISION.
    *>    INPUT-OUTPUT SECTION.
    *>    FILE-CONTROL.
    *>        SELECT SYSIN ASSIGN TO KEYBOARD ORGANIZATION LINE SEQUENTIAL.
    *>    DATA DIVISION.
    *>    FILE SECTION.
    *>        FD SYSIN.
    *>            01 INP PIC X(2000000).
    *>    WORKING-STORAGE SECTION.
    *>        01 WK.
    *>            03 I PIC 9(18).
    *>            03 J PIC 9(18).
    *>            03 K PIC 9(18).
    *>            03 N PIC 9(18).
    *>            03 M PIC 9(18).
    *>            03 ANS PIC 9(18).
    *>            03 ANS-Z PIC Z(17)9.
    *>            03 PT PIC 9(18) VALUE 1.
    *>    PROCEDURE DIVISION.
    *>        ACCEPT N.
    *>        OPEN INPUT SYSIN.
    *>        READ SYSIN.
    *>        CLOSE SYSIN.
    *>        UNSTRING INP DELIMITED BY SPACE INTO N M.
    *>        MOVE ANS TO ANS-Z.
    *>        DISPLAY FUNCTION TRIM(ANS-Z).
    *>        STOP RUN.
    *>    END PROGRAM SUB.
