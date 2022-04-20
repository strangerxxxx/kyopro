       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATCODER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYSIN ASSIGN TO KEYBOARD ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
           FD SYSIN.
               01 INP PIC X(2200000).
       WORKING-STORAGE SECTION.
           01 WK.
               03 INP-WK PIC X(8190).
               03 I BINARY-DOUBLE SIGNED VALUE 0.
               03 J BINARY-DOUBLE SIGNED VALUE 0.
               03 K BINARY-DOUBLE SIGNED VALUE 0.
               03 N BINARY-DOUBLE SIGNED VALUE 0.
               03 M BINARY-DOUBLE SIGNED VALUE 0.
               03 FL FLOAT-LONG VALUE 0.
               03 ANS BINARY-DOUBLE SIGNED VALUE 0.
               03 ANS-Z PIC -Z(20)9.
               03 MD BINARY-LONG SIGNED VALUE 998244353.
               03 INF BINARY-DOUBLE SIGNED VALUE 9223372036854775807.
               03 PT BINARY-DOUBLE SIGNED VALUE 1.
       PROCEDURE DIVISION.
           ACCEPT N.
           ACCEPT INP-WK.
           UNSTRING INP-WK DELIMITED BY SPACE INTO N M.
           OPEN INPUT SYSIN.
           READ SYSIN.
           CLOSE SYSIN.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               ACCEPT INP-WK
           END-PERFORM.
           IF BOOL DISPLAY "Yes" ELSE DISPLAY "No" END-IF.
           MOVE ANS TO ANS-Z.
           DISPLAY FUNCTION TRIM(ANS-Z).
           STOP RUN.
       END PROGRAM ATCODER.

      *>   SPLIT
           UNSTRING INP DELIMITED BY SPACE INTO N M.

      *>   SPLIT TO LIST
               03 I BINARY-DOUBLE SIGNED VALUE 0.
               03 N BINARY-DOUBLE SIGNED VALUE 0.
               03 PT BINARY-DOUBLE SIGNED VALUE 1.
           01 AL.
               03 AI OCCURS 200000 TIMES.
                   05 A BINARY-LONG SIGNED VALUE 0.
       PROCEDURE DIVISION.
           OPEN INPUT SYSIN.
           READ SYSIN.
           MOVE 1 TO PT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               UNSTRING INP DELIMITED BY SPACE INTO A(I) WITH POINTER PT
           END-PERFORM.
           CLOSE SYSIN.

      *>   SORT
           01 AL.
               03 AI OCCURS 0 TO 200000 TIMES DEPENDING ON N.
                   05 A BINARY-LONG SIGNED VALUE 0.
           SORT AI ON ASCENDING KEY A.
           SORT AI ON DESCENDING KEY A.

      *>   LEN
           MOVE FUNCTION STORED-CHAR-LENGTH(S) TO N.
           MOVE FUNCTION STORED-CHAR-LENGTH(INP-WK) TO N.

      *>   SPLIT TO LIST 2D
           01 AL.
               03 AI OCCURS 200000 TIMES.
                   05 AJ OCCURS 200000 TIMES.
                       07 A BINARY-LONG SIGNED VALUE 0.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               READ SYSIN
               MOVE 1 TO PT
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
                   UNSTRING INP DELIMITED BY SPACE INTO A(I J)
                                               WITH POINTER PT
               END-PERFORM
           END-PERFORM.
           
      *>   OUTPUT WITHOUT ADVANCING
           DISPLAY ANS WITH NO ADVANCING.

      *>   BIT
           CALL "CBL_OR" USING I, J, BY VALUE 8.
           CALL "CBL_AND" USING I, J, BY VALUE 8.
           CALL "CBL_NOT" USING I, J, BY VALUE 8.
           CALL "CBL_XOR" USING I, J, BY VALUE 8.

      *>   BIT全探索
           COMPUTE M = 2 ** N.
           PERFORM VARYING I FROM 0 BY 1 UNTIL I >= M
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               COMPUTE K = 2 ** (J - 1)
               CALL "CBL_AND" USING I, K, BY VALUE 8
               IF K >= 1
      *>           処理を書く
               END-IF
           END-PERFORM
           END-PERFORM.

      *>   STRING LIST
           01 SL.
               03 SI OCCURS 1 TO 200000 TIMES DEPENDING ON N.
                   05 S PIC X(1).
           01 SL.
               03 SI OCCURS 1000 TIMES.
                   05 SJ OCCURS 1000 TIMES.
                       07 S PIC X(1).

      *>   MOVE OVER 19 CHARS
           01 INPUT-WK.
               03 INPUT-S PIC X(22).
               03 INPUT-I BINARY-DOUBLE SIGNED VALUE 0.
               03 INPUT-M BINARY-DOUBLE UNSIGNED VALUE 0.
       INPUT-DOUBLE SECTION.
           IF FUNCTION STORED-CHAR-LENGTH(INPUT-S) < 18
               MOVE INPUT-S TO INPUT-I
           ELSE
               IF INPUT-S(1:1) = "-"
                   COMPUTE INPUT-M
                             = FUNCTION STORED-CHAR-LENGTH(INPUT-S) - 18
                   COMPUTE INPUT-I
                    = FUNCTION NUMVAL(INPUT-S(1:INPUT-M)) * 10 ** 18
                       + FUNCTION NUMVAL(INPUT-S(INPUT-M + 1:18)) * (-1)
               ELSE
                   COMPUTE INPUT-M
                             = FUNCTION STORED-CHAR-LENGTH(INPUT-S) - 18
                   COMPUTE INPUT-I
                    = FUNCTION NUMVAL(INPUT-S(1:INPUT-M)) * 10 ** 18
                       + FUNCTION NUMVAL(INPUT-S(INPUT-M + 1:18))
               END-IF
           END-IF
       EXIT SECTION.

      *>   EVALUATE
           EVALUATE 式
           WHEN 値 文
           WHEN 値 文
           OTHER 文
           END-EVALUATE.

      *>      SECTION
       HOGE SECTION.
       EXIT.
