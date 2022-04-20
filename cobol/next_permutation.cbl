           01 WK.
               03 I BINARY-DOUBLE SIGNED VALUE 0.
               03 N BINARY-DOUBLE SIGNED VALUE 0.
           01 PERM-WK.
               03 PERM-I BINARY-DOUBLE SIGNED VALUE 0.
               03 PERM-J BINARY-DOUBLE SIGNED VALUE 0.
               03 PERM-P BINARY-DOUBLE SIGNED VALUE 0.
               03 PERM-Q BINARY-DOUBLE SIGNED VALUE 0.
               03 PERM-TMP BINARY-DOUBLE SIGNED VALUE 0.
               03 NN BINARY-DOUBLE SIGNED VALUE 0.
               03 COND BINARY-DOUBLE SIGNED VALUE 1.
           01 AL.
               03 AI OCCURS 1 TO 20 TIMES DEPENDING ON N.
                   05 A BINARY-DOUBLE SIGNED VALUE 0.

           PERFORM UNTIL COND = 0
               DISPLAY AL
               PERFORM NEXT_PERMUTATION
           END-PERFORM.
       NEXT_PERMUTATION SECTION.
           MOVE 0 TO COND.
           COMPUTE NN = N - 1.
           PERFORM VARYING PERM-I FROM NN BY -1 UNTIL PERM-I <= 0
               IF A(PERM-I) < A(PERM-I + 1) THEN
                   MOVE 1 TO COND
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           IF COND = 1 THEN
               PERFORM VARYING PERM-J FROM N BY -1
                       UNTIL A(PERM-I) < A(PERM-J)
               END-PERFORM
               MOVE A(PERM-I) TO PERM-TMP
               MOVE A(PERM-J) TO A(PERM-I)
               MOVE PERM-TMP TO A(PERM-J)
               COMPUTE PERM-P = PERM-I + 1
               MOVE N TO PERM-Q
               PERFORM UNTIL PERM-P >= PERM-Q
                   MOVE A(PERM-P) TO PERM-TMP
                   MOVE A(PERM-Q) TO A(PERM-P)
                   MOVE PERM-TMP TO A(PERM-Q)
                   ADD 1 TO PERM-P
                   SUBTRACT 1 FROM PERM-Q
               END-PERFORM
           END-IF.
       EXIT SECTION.
