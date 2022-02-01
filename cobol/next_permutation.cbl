           01 WK.
               03 I PIC 9(18).
               03 N PIC 9(18).
           01 AL.
               03 AI OCCURS 1 TO 20 TIMES DEPENDING ON N.
                   05 A PIC 9(10).
           01 PERM-WK.
               03 PERM-I PIC 9(18).
               03 PERM-J PIC 9(18).
               03 PERM-P PIC 9(18).
               03 PERM-Q PIC 9(18).
               03 PERM-TMP PIC 9(18).
               03 NN PIC 9(18).
               03 COND PIC 9(1) VALUE 1.

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
